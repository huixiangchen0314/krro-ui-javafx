(ns top.kzre.krro.ui.javafx.tags
  "JavaFX 标签渲染多方法。不再传递 parent 参数。"
  (:require [top.kzre.krro.core.command :as cmd]
            [top.kzre.krro.core.project :as proj])
  (:import [javafx.scene.control Button CheckBox ColorPicker ComboBox Label
                                 Labeled MenuBar Menu MenuItem Separator Slider TextArea TextField ToolBar]
           [javafx.scene.layout HBox VBox Priority]
           [javafx.event EventHandler]
           [javafx.scene Node]))

(defn- apply-style [^Node node style]
  (let [css-str (->> (for [[k v] style]
                       (str "-fx-" (name k) ": " v ";"))
                     (clojure.string/join " "))]
    (.setStyle node css-str)))

(defn update-attrs [^Node node old-attrs new-attrs]
  ;; 更新样式
  (let [old-style (:style old-attrs)
        new-style (:style new-attrs)]
    (when (not= old-style new-style)
      (if new-style
        (apply-style node new-style)
        (.setStyle node ""))))
  ;; 更新可见性
  (when (and (contains? new-attrs :visible?) (not= (:visible? old-attrs) (:visible? new-attrs)))
    (.setVisible node (boolean (:visible? new-attrs))))
  ;; 更新禁用状态
  (when (and (contains? new-attrs :disabled?) (not= (:disabled? old-attrs) (:disabled? new-attrs)))
    (.setDisable node (boolean (:disabled? new-attrs))))
  ;; 更新文本（如果节点是 Labeled 类型）
  (when (and (instance? Labeled node) (contains? new-attrs :text)
             (not= (:text old-attrs) (:text new-attrs)))
    (.setText ^Labeled node (str (:text new-attrs))))
  ;; 命令绑定暂不更新（一般不会变）
  )

;; ── 通用属性处理 ──────────────────────────────────
(defn- apply-common-attrs
  [^Node node attrs]
  (when-let [id (:id attrs)] (.setId node (name id)))
  (when-let [style (:style attrs)] (apply-style node style))
  (when (contains? attrs :visible?) (.setVisible node (boolean (:visible? attrs))))
  (when (contains? attrs :disabled?) (.setDisable node (boolean (:disabled? attrs))))
  node)



(defn bind-command [^Node node attrs]
  (when-let [cmd-id (:on-command attrs)]
    (.setOnAction node
                  (reify EventHandler
                    (handle [this event]
                      (apply cmd/execute-command! cmd-id (:command-args attrs))))))
  node)

;; ── 标签多方法（仅接收 element 和 renderer）────────
(defmulti render-tag (fn [element _renderer] (first element)))

(defmethod render-tag :v-box [[_ attrs & children] renderer]
  (let [box (VBox.)]
    (apply-common-attrs box attrs)
    (doseq [child children]
      (when-let [node (render-tag child renderer)]
        (.add (.getChildren box) node)))
    box))

(defmethod render-tag :h-box [[_ attrs & children] renderer]
  (let [box (HBox.)]
    (apply-common-attrs box attrs)
    (doseq [child children]
      (when-let [node (render-tag child renderer)]
        (.add (.getChildren box) node)))
    box))

(defmethod render-tag :label [[_ attrs] _]
  (let [lbl (Label. (or (:text attrs) ""))]
    (apply-common-attrs lbl attrs)
    lbl))

(defmethod render-tag :button [[_ attrs] _]
  (let [btn (Button. (or (:text attrs) ""))]
    (apply-common-attrs btn attrs)
    (bind-command btn attrs)
    btn))

(defmethod render-tag :text-field [[_ attrs] _]
  (let [tf (TextField. (or (:text attrs) ""))]
    (apply-common-attrs tf attrs)
    (when-let [cmd-id (:on-command attrs)]
      (.setOnAction tf
                    (reify EventHandler
                      (handle [this event]
                        (cmd/execute-command! cmd-id (:command-args attrs))))))
    tf))

(defmethod render-tag :text-area [[_ attrs] _]
  (let [ta (TextArea. (or (:text attrs) ""))]
    (apply-common-attrs ta attrs)
    ta))

(defmethod render-tag :checkbox [[_ attrs] _]
  (let [cb (CheckBox. (or (:text attrs) ""))]
    (apply-common-attrs cb attrs)
    (when-let [cmd-id (:on-command attrs)]
      (.setOnAction cb
                    (reify EventHandler
                      (handle [this event]
                        (cmd/execute-command! cmd-id (:command-args attrs))))))
    cb))

(defmethod render-tag :slider [[_ attrs] _]
  (let [sl (Slider. (double (or (:min attrs) 0))
                    (double (or (:max attrs) 100))
                    (double (or (:value attrs) 50)))]
    (apply-common-attrs sl attrs)
    sl))

(defmethod render-tag :combo-box [[_ attrs] _]
  (let [cb (ComboBox.)]
    (apply-common-attrs cb attrs)
    (when-let [items (:items attrs)] (.addAll (.getItems cb) items))
    (when-let [val (:value attrs)] (.setValue cb val))
    cb))

(defmethod render-tag :color-picker [[_ attrs] _]
  (let [cp (ColorPicker.)]
    (apply-common-attrs cp attrs)
    cp))

(defmethod render-tag :separator [_ _]
  (Separator.))

;; ── 菜单栏/工具栏 ──────────────────────────────
(defmethod render-tag :menu-bar [[_ attrs & children] renderer]
  (let [bar (MenuBar.)]
    (apply-common-attrs bar attrs)
    (doseq [child children]
      (when-let [node (render-tag child renderer)]
        (.add (.getMenus bar) node)))
    bar))

(defmethod render-tag :menu [[_ attrs & children] renderer]
  (let [menu (Menu. (or (:text attrs) ""))]
    (apply-common-attrs menu attrs)
    (doseq [child children]
      (when-let [node (render-tag child renderer)]
        (.add (.getItems menu) node)))
    menu))

(defmethod render-tag :menu-item [[_ attrs] _]
  (let [item (MenuItem. (or (:text attrs) ""))]
    (apply-common-attrs item attrs)
    (bind-command item attrs)
    item))

(defmethod render-tag :tool-bar [[_ attrs & children] renderer]
  (let [bar (ToolBar.)]
    (apply-common-attrs bar attrs)
    (doseq [child children]
      (when-let [node (render-tag child renderer)]
        (.add (.getItems bar) node)))
    bar))

(defmethod render-tag :default [element _]
  (Label. (str "(unknown: " (first element) ")")))