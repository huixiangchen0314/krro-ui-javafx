(ns top.kzre.krro.ui.javafx.impl
  "JavaFX 平台实现：元素工厂与节点渲染器。适配层，调用 tags 创建节点，处理通用属性。
   多方法根据容器类型的全限定名字符串进行分派，避免编译期加载 JavaFX 类。"
  (:require
    [clojure.string :as str]
    [top.kzre.krro.ui.core.bind :as bind]
    [top.kzre.krro.ui.core.protocol :as proto]
    [top.kzre.krro.ui.javafx.renderer :as renderer]
    [top.kzre.krro.ui.javafx.tags :as tags])
  (:import
   [javafx.scene Node]
   [javafx.scene.control Label TextField]))

;; ── 通用属性处理 ──────────────────────────────────────
(defn- apply-style [^Node node style-map]
  (when style-map
    (let [css-str (->> (for [[k v] style-map]
                         (str "-fx-" (name k) ": " v ";"))
                       (str/join " "))]
      (.setStyle node css-str))))

(defn- apply-common-attrs [^Node node props]
  (when-let [id (:key props)] (.setId node (name id)))
  (when-let [style (:style props)] (apply-style node style))
  (when (contains? props :visible?) (.setVisible node (boolean (:visible? props))))
  (when (contains? props :disabled?) (.setDisable node (boolean (:disabled? props))))
  node)

(defn- update-attrs [^Node node old-props new-props]
  (when (not= (:style old-props) (:style new-props))
    (if-let [style (:style new-props)]
      (apply-style node style)
      (.setStyle node "")))
  (when (not= (:visible? old-props) (:visible? new-props))
    (.setVisible node (boolean (:visible? new-props))))
  (when (not= (:disabled? old-props) (:disabled? new-props))
    (.setDisable node (boolean (:disabled? new-props))))
  (when (and (instance? Label node) (not= (:content old-props) (:content new-props)))
    (.setText ^Label node (or (:content new-props) "")))
  (when (and (instance? TextField node) (not= (:content old-props) (:content new-props)))
    (.setText ^TextField node (or (:content new-props) ""))))

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
          (apply-common-attrs node props)
          (when hooks
            (doseq [[k f] hooks]
              (proto/add-hook! vnode k f)))
          ;; 注册项目原子绑定
          (when bindings
            (let [manager (or (:bind-manager bindings) bind/*default-bind-manager*)]
              (doseq [b (:items bindings)]
                (bind/register! manager node (:path b) (:apply-fn b) (dissoc b :path :apply-fn)))))
          ;; 注册 Frame 参数绑定
          (when frame-bindings
            (let [manager (renderer/ensure-frame-bind-manager f)]
              (doseq [b (:items frame-bindings)]
                (bind/register! manager node (:path b) (:apply-fn b) (dissoc b :path :apply-fn)))))
          node)
        ;; 传统方式
        (let [^Node node result]
          (apply-common-attrs node props)
          node))))
  (update-properties [_ element old-vnode new-vnode]
    (if (-> new-vnode proto/node-hooks :on-update)
      element   ;; 组件自有 on-update，不再做通用属性更新
      (let [old-props (proto/node-props old-vnode)
            new-props (proto/node-props new-vnode)]
        (update-attrs element old-props new-props)
        (bind/refresh! element)))))


;; ── 多方法：根据容器类型的全限定字符串分派 ─────────
(defmulti append-node  (fn [parent child]          (when parent (.getName (class parent)))))
(defmulti insert-node  (fn [parent child index]    (when parent (.getName (class parent)))))
(defmulti remove-node  (fn [parent child]          (when parent (.getName (class parent)))))
(defmulti replace-node (fn [parent old new]        (when parent (.getName (class parent)))))
(defmulti move-node    (fn [parent child target]   (when parent (.getName (class parent)))))

;; 默认实现（普通 Parent / Pane）
(defmethod append-node :default [parent child]
  (.add (.getChildren parent) child))
(defmethod insert-node :default [parent child index]
  (.add (.getChildren parent) index child))
(defmethod remove-node :default [parent child]
  (.remove (.getChildren parent) child))
(defmethod replace-node :default [parent old new]
  (let [children (.getChildren parent)
        idx (.indexOf children old)]
    (when (>= idx 0) (.set children idx new))))
(defmethod move-node :default [parent child target-index]
  (let [children (.getChildren parent)
        idx (.indexOf children child)]
    (when (and (>= idx 0) (not= idx target-index))
      (.remove children child)
      (.add children target-index child))))

;; MenuBar
(defmethod append-node "javafx.scene.control.MenuBar" [parent child]
  (.add (.getMenus parent) child))
(defmethod insert-node "javafx.scene.control.MenuBar" [parent child index]
  (.add (.getMenus parent) index child))
(defmethod remove-node "javafx.scene.control.MenuBar" [parent child]
  (.remove (.getMenus parent) child))
(defmethod replace-node "javafx.scene.control.MenuBar" [parent old new]
  (let [items (.getMenus parent)
        idx (.indexOf items old)]
    (when (>= idx 0) (.set items idx new))))
(defmethod move-node "javafx.scene.control.MenuBar" [parent child target-index]
  (let [items (.getMenus parent)
        idx (.indexOf items child)]
    (when (and (>= idx 0) (not= idx target-index))
      (.remove items child)
      (.add items target-index child))))

;; Menu
(defmethod append-node "javafx.scene.control.Menu" [parent child]
  (.add (.getItems parent) child))
(defmethod insert-node "javafx.scene.control.Menu" [parent child index]
  (.add (.getItems parent) index child))
(defmethod remove-node "javafx.scene.control.Menu" [parent child]
  (.remove (.getItems parent) child))
(defmethod replace-node "javafx.scene.control.Menu" [parent old new]
  (let [items (.getItems parent)
        idx (.indexOf items old)]
    (when (>= idx 0) (.set items idx new))))
(defmethod move-node "javafx.scene.control.Menu" [parent child target-index]
  (let [items (.getItems parent)
        idx (.indexOf items child)]
    (when (and (>= idx 0) (not= idx target-index))
      (.remove items child)
      (.add items target-index child))))

;; ToolBar
(defmethod append-node "javafx.scene.control.ToolBar" [parent child]
  (.add (.getItems parent) child))
(defmethod insert-node "javafx.scene.control.ToolBar" [parent child index]
  (.add (.getItems parent) index child))
(defmethod remove-node "javafx.scene.control.ToolBar" [parent child]
  (.remove (.getItems parent) child))
(defmethod replace-node "javafx.scene.control.ToolBar" [parent old new]
  (let [items (.getItems parent)
        idx (.indexOf items old)]
    (when (>= idx 0) (.set items idx new))))
(defmethod move-node "javafx.scene.control.ToolBar" [parent child target-index]
  (let [items (.getItems parent)
        idx (.indexOf items child)]
    (when (and (>= idx 0) (not= idx target-index))
      (.remove items child)
      (.add items target-index child))))

;; TabPane
(defmethod append-node "javafx.scene.control.TabPane" [parent child]
  (.add (.getTabs parent) child))
(defmethod insert-node "javafx.scene.control.TabPane" [parent child index]
  (.add (.getTabs parent) index child))
(defmethod remove-node "javafx.scene.control.TabPane" [parent child]
  (.remove (.getTabs parent) child))
(defmethod replace-node "javafx.scene.control.TabPane" [parent old new]
  (let [tabs (.getTabs parent)
        idx (.indexOf tabs old)]
    (when (>= idx 0) (.set tabs idx new))))
(defmethod move-node "javafx.scene.control.TabPane" [parent child target-index]
  (let [tabs (.getTabs parent)
        idx (.indexOf tabs child)]
    (when (and (>= idx 0) (not= idx target-index))
      (.remove tabs child)
      (.add tabs target-index child))))

;; ── 节点操作渲染器（krro-ui-core 的 IRenderer）────────
(defrecord JavaFxNodeRenderer []
  proto/IRenderer
  (append-child [_ parent child]       (append-node parent child))
  (insert-child [_ parent child index] (insert-node parent child index))
  (remove-child [_ parent child]       (remove-node parent child))
  (replace-child [_ parent old new]    (replace-node parent old new))
  (move-child [_ parent child idx]     (move-node parent child idx)))