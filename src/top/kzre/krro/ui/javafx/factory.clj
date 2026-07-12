(ns top.kzre.krro.ui.javafx.factory
  "JavaFX 平台实现：元素工厂与节点渲染器。适配层，调用 tags 创建节点，处理通用属性。
   多方法根据容器类型的全限定名字符串进行分派，避免编译期加载 JavaFX 类。"
  (:require
   [clojure.string :as str]
   [top.kzre.krro.core.frame :as frame]
   [top.kzre.krro.ui.core.bind :as bind]
   [top.kzre.krro.ui.core.protocol :as proto]
   [top.kzre.krro.ui.javafx.drag :as drag]
   [top.kzre.krro.ui.javafx.renderer :as renderer]
   [top.kzre.krro.ui.javafx.tags :as tags])
  (:import
    [javafx.scene Node]
    [javafx.scene.control CheckBox Label TextField]))

;; ── 通用属性处理 ──────────────────────────────────────
(defn- apply-style [^Node node style-map]
  (when style-map
    (let [css-str (->> (for [[k v] style-map]
                         (str "-fx-" (name k) ": " v ";"))
                       (str/join " "))]
      (.setStyle node css-str))))

(defn- apply-style-class [^Node node class-val]
  (when class-val
    (let [classes (if (vector? class-val) class-val [class-val])]
      (.addAll (.getStyleClass node) (map name classes)))))



(defn- apply-common-attrs [^Node node props]
  (when-let [id (:key props)] (.setId node (name id)))
  (when-let [style (:style props)] (apply-style node style))
  (when (contains? props :class) (apply-style-class node (:class props)))
  (when (contains? props :visible?) (.setVisible node (boolean (:visible? props))))
  (when (contains? props :disabled?) (.setDisable node (boolean (:disabled? props))))
  node)


(defn- update-attrs [^Node node old-props new-props]
  (when (not= (:style old-props) (:style new-props))
    (if-let [style (:style new-props)]
      (apply-style node style)
      (.setStyle node "")))
  (when (not= (:class old-props) (:class new-props))
    ;; 移除所有旧类，添加新类
    (.clear (.getStyleClass node))
    (apply-style-class node (:class new-props)))
  (when (not= (:visible? old-props) (:visible? new-props))
    (.setVisible node (boolean (:visible? new-props))))
  (when (not= (:disabled? old-props) (:disabled? new-props))
    (.setDisable node (boolean (:disabled? new-props))))
  (when (and (instance? Label node) (not= (:content old-props) (:content new-props)))
    (.setText ^Label node (or (:content new-props) "")))
  (when (and (instance? CheckBox node) (not= (:checked? old-props) (:checked? new-props)))
    (.setSelected ^CheckBox node (boolean (:checked? new-props))))
  (when (and (instance? TextField node) (not= (:content old-props) (:content new-props)))
    (.setText ^TextField node (or (:content new-props) ""))))

(defn- setup-drag!
  "根据 props 中的 :drag-source / :drag-target 配置拖拽。"
  [^Node node props]
  (when-let [ds (:drag-source props)]
    (drag/setup-drag-source! node ds))
  (when-let [dt (:drag-target props)]
    (drag/setup-drag-target! node dt)))

(defn- apply-props!
  "一次性应用所有静态属性（样式、拖拽等）。"
  [^Node node props]
  (apply-common-attrs node props)
  (setup-drag! node props))

(defn- register-bindings!
  "注册项目绑定与 Frame 绑定。"
  [^Node node bindings frame-bindings f]
  (when bindings
    (let [manager (or (:bind-manager bindings) bind/*default-bind-manager*)]
      (doseq [b bindings]
        (bind/register! manager node (:path b) (:apply-fn b) (dissoc b :path :apply-fn)))))
  (when frame-bindings
    (let [manager (renderer/ensure-frame-bind-manager f)]
      (doseq [b frame-bindings]
        (bind/register! manager node (:path b) (:apply-fn b) (dissoc b :path :apply-fn))))))

(defn- setup-hooks!
  "安装 vnode 生命週期钩子。"
  [vnode hooks]
  (when hooks
    (doseq [[k f] hooks]
      (proto/add-hook! vnode k f))))

;; ── 元素工厂 ────────────────────────────────────────
(defrecord JavaFxElementFactory []
  proto/IElementFactory
  (create-element [_ vnode f]
    (let [tag   (proto/node-type vnode)
          props (proto/node-props vnode)
          result (tags/create-element tag props f)]
      (if (map? result)
        (let [^Node node (:node result)
              hooks (:hooks result)
              bindings (:bindings result)
              frame-bindings (:frame-bindings result)]
          ;; 1. 应用静态属性（样式、拖拽等）
          (apply-props! node props)
          ;; 2. 注册钩子
          (setup-hooks! vnode hooks)
          ;; 3. 注册绑定
          (register-bindings! node bindings frame-bindings f)
          node)
        ;; 传统返回值（仅节点）
        (let [^Node node result]
          (apply-props! node props)
          node))))
  (update-properties [_ element old-vnode new-vnode]
    (if (-> new-vnode proto/node-hooks :on-update)
      element   ;; 组件自有 on-update，不再做通用属性更新
      (let [old-props (proto/node-props old-vnode)
            new-props (proto/node-props new-vnode)]
        (update-attrs element old-props new-props)
       )))
  (destroy-element [_ vnode f]
    (when-let [element (proto/node-element vnode)]
      (bind/unregister! element)   ;; 清除项目原子绑定
      (when-let [fm (frame/param f renderer/frame-bind-manager-key)]
        (bind/unregister! fm element)))))
