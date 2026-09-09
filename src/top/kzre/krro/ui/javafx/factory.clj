(ns top.kzre.krro.ui.javafx.factory
  "JavaFX 平台实现：元素工厂与节点渲染器。适配层，调用 tags 创建节点，处理通用属性。
   多方法根据容器类型的全限定名字符串进行分派，避免编译期加载 JavaFX 类。"
  (:require
   [clojure.string :as str]
   [top.kzre.krro.ui.core.bind :as bind]
   [top.kzre.krro.ui.core.protocol :as proto]
   [top.kzre.krro.ui.core.spec.stylesheet :as stylesheet]
   [top.kzre.krro.ui.javafx.drag :as drag]
   [top.kzre.krro.ui.javafx.renderer :as renderer]
   [top.kzre.krro.ui.javafx.stylesheet :as javafx.stylesheet]
   [top.kzre.krro.ui.javafx.tags :as tags]
   [top.kzre.krro.ui.javafx.util :as javafx.util]
   [taoensso.timbre :as log])
  (:import
    (java.io File)
    (java.util Collection)
    (javafx.scene Node Parent)
    (javafx.scene.control
      Button
      CheckBox
      ComboBox
      Hyperlink
      Label
      MenuItem
      ProgressBar
      RadioButton
      Slider
      TextArea
      TextField)
    [javafx.scene.image ImageView]
    [javafx.scene.layout HBox VBox]))

;; ── 通用属性处理 ──────────────────────────────────────
(defn- set-style [^Node node style-map]
  (when style-map
    (let [css-str (->> (for [[k v] style-map]
                         (str "-fx-" (name k) ": " v ";"))
                       (str/join " "))]
      (.setStyle node css-str))))

(defn- set-class [^Node node class-val]
  (when class-val
    (let [classes (if (vector? class-val) class-val [class-val])]
      (.addAll ^Collection (.getStyleClass node) (map name classes)))))


(defn- attrs-diff! [element old-props new-props]

  ;; 样式
  (when (not= (:style old-props) (:style new-props))
    (if-let [style (:style new-props)]
      (set-style element style)
      (.setStyle element "")))
  ;; 类
  (when (not= (:class old-props) (:class new-props))
    (.clear (.getStyleClass element))
    (set-class element (:class new-props)))

  ;; 样式表
  ;; 样式表：仅对 Parent 类型节点生效
  (when (and (instance? Parent element)
             (not= (:stylesheet old-props) (:stylesheet new-props)))
    (let [styles (.getStylesheets element)
          new-styles (:stylesheet new-props)]
      (.clear styles)
      (when new-styles
        (let [stylesheets (if (vector? new-styles) new-styles [new-styles])]
          (doseq [s stylesheets]
            (try
              (if (and (string? s) (str/ends-with? s ".edn"))
                (let [edn (stylesheet/load-stylesheet-edn s)
                      compiled (javafx.stylesheet/compile-stylesheet edn)
                      temp-file (doto (File/createTempFile "krr-style-" ".css")
                                  (.deleteOnExit)
                                  (spit compiled))]
                  (.add styles (.toString (.toURI temp-file))))
                (.add styles s))
              (catch Exception e
                (log/error e "Failed to load stylesheet" s))))))))

  ;; 可见性和禁用
  (when (not= (:visible old-props) (:visible new-props))
    (.setVisible element (boolean (:visible new-props))))
  (when (not= (:disabled old-props) (:disabled new-props))
    (.setDisable element (boolean (:disabled new-props))))

  ;; 拖拽：先清理旧的，再设置新的
  (when (not= (:drag-source old-props) (:drag-source new-props))
    (drag/clear-drag-source! element))
  (when (not= (:drag-target old-props) (:drag-target new-props))
    (drag/clear-drag-target! element))
  (when-let [ds (:drag-source new-props)]
    (drag/setup-drag-source! element ds))
  (when-let [dt (:drag-target new-props)]
    (drag/setup-drag-target! element dt))


  ;; 组件特定
  (cond
    (or (instance? VBox element) (instance? HBox element))
    (let [new-alignment (:alignment new-props :top-left)]
      (when (not= (:alignment old-props) new-alignment)
        (.setAlignment element (javafx.util/kw->pos new-alignment))))

    (instance? Label element)
    (when (not= (:content old-props) (:content new-props))
      (.setText ^Label element (or (:content new-props) "")))

    (instance? Button element)
    (when (not= (:content old-props) (:content new-props))
      (.setText ^Button element (or (:content new-props) "")))

    (instance? CheckBox element)
    (do
      (when (not= (:content old-props) (:content new-props))
        (.setText ^CheckBox element (or (:content new-props) "")))
      (when (not= (:checked old-props) (:checked new-props))
        (.setSelected ^CheckBox element (boolean (:checked new-props)))))

    (instance? RadioButton element)
    (do
      (when (not= (:content old-props) (:content new-props))
        (.setText ^RadioButton element (or (:content new-props) "")))
      (when (not= (:checked old-props) (:checked new-props))
        (.setSelected ^RadioButton element (boolean (:checked new-props)))))

    (instance? TextField element)
    (when (not= (:content old-props) (:content new-props))
      (.setText ^TextField element (or (:content new-props) "")))

    (instance? TextArea element)
    (when (not= (:content old-props) (:content new-props))
      (.setText ^TextArea element (or (:content new-props) "")))

    (instance? ComboBox element)
    (do
      (when (not= (:items old-props) (:items new-props))
        (let [^ComboBox cb element
              ^Collection cur-items (.getItems cb)]
          (.clear cur-items)
          (when-let [items (:items new-props)]
            (.addAll cur-items items))))
      (when (not= (:value old-props) (:value new-props))
        (.setValue ^ComboBox element (:value new-props))))

    (instance? Slider element)
    (when (not= (:value old-props) (:value new-props))
      (.setValue ^Slider element (double (:value new-props))))

    (instance? ProgressBar element)
    (when (not= (:value old-props) (:value new-props))
      (.setProgress ^ProgressBar element (double (:value new-props))))

    (instance? ImageView element)
    (when (not= (:src old-props) (:src new-props))
      (.setImage ^ImageView element (javafx.scene.image.Image. ^String (:src new-props))))

    (instance? Hyperlink element)
    (when (not= (:content old-props) (:content new-props))
      (.setText ^Hyperlink element (or (:content new-props) "")))

    (instance? MenuItem element)        ; MenuItem 不是 Node，但可能被特殊处理
    (when (not= (:content old-props) (:content new-props))
      (.setText ^MenuItem element (or (:content new-props) "")))))

(defn- register-bindings!
  "注册项目绑定与 Frame 绑定。"
  [^Node node bind bindf frame]
  (when bind
    (let [{:keys [path apply-fn getter bind-ctx]} bind
          ctx (or bind-ctx bind/*default-bind-manager*)]
      (bind/register! ctx node apply-fn :path path :getter getter)))
  (when bindf
    (let [{:keys [path apply-fn getter]} bindf
          ctx (renderer/ensure-frame-bind-ctx frame)]
      (bind/register! ctx node apply-fn :path path :getter getter))))

(defn- setup-hooks!
  "安装 vnode 生命週期钩子。"
  [vnode hooks]
  (when hooks
    (doseq [[k f] hooks]
      (proto/add-hook! vnode k f))))

;; ── 元素工厂 ────────────────────────────────────────
(defrecord JavaFxElementFactory []
  proto/IElementFactory
  (create-element [_ vnode frame]
    (let [tag   (proto/node-type vnode)
          props (proto/node-props vnode)
          result (tags/create-element tag props frame)]
      (if (map? result)
        (let [{:keys [node hooks binding frame-binding]} result]
          ;; 1. 应用静态属性（样式、拖拽等）
          (attrs-diff! node {} props)
          ;; 2. 注册钩子
          (setup-hooks! vnode hooks)
          ;; 3. 注册绑定
          (register-bindings! node binding frame-binding frame)
          node)
        ;; 传统返回值（仅节点）
        (let [^Node node result]
          (attrs-diff! node {} props)
          node))))
  (update-properties [_ element old-vnode new-vnode]
    (if (-> new-vnode proto/node-hooks :on-update)
      element   ;; 组件自有 on-update，不再做通用属性更新
      (let [old-props (proto/node-props old-vnode)
            new-props (proto/node-props new-vnode)]
        (attrs-diff! element old-props new-props)
       )))
  (destroy-element [_ vnode f]
    (when-let [element (proto/node-element vnode)]
      (bind/unregister! element)   ;; 清除项目原子绑定
      (when-let [fm (renderer/get-frame-bind-manager f)]
        (bind/unregister! fm element)))))
