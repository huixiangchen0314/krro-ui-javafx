(ns top.kzre.krro.ui.javafx.tags
  "JavaFX 标签多方法。每个标签只创建对应的平台节点，返回节点和可选的绑定描述。
   数据绑定通过 :bindings 和 :frame-bindings 声明，由工厂统一注册。
   双向绑定直接使用传入的 frame 参数操作源 atom。"
  (:require [top.kzre.krro.core.command :as cmd]
            [top.kzre.krro.core.project :as proj]
            [top.kzre.krro.core.frame :as frame]
            [top.kzre.krro.ui.core.vnode :as vnode])
  (:import [javafx.beans.value ChangeListener]
           [javafx.event EventHandler]
           [javafx.scene.control Button CheckBox ColorPicker ComboBox Hyperlink Label ListView
                                 Menu MenuBar MenuItem ProgressBar RadioButton ScrollPane Separator Slider
                                 SplitPane Tab TabPane TextArea TextField ToolBar TreeView]
           [javafx.scene.layout HBox VBox]))

(defmulti create-element (fn [tag _props _frame] tag))

(defn- handle-action [node props event-key]
  (when-let [on-spec (vnode/event props event-key)]
    (.setOnAction node
                  (reify EventHandler
                    (handle [_ _]
                      (if (fn? on-spec)
                        (on-spec node)
                        (cmd/execute-command! on-spec)))))))

;; ── 布局容器 ──────────────────────────────────────────
(defmethod create-element :block [_ props _]
  {:node (if (= (:direction props :vertical) :vertical) (VBox.) (HBox.))})
(defmethod create-element :split-pane [_ _ _] {:node (SplitPane.)})
(defmethod create-element :scroll [_ _ _] {:node (ScrollPane.)})
(defmethod create-element :tab-panel [_ _ _] {:node (TabPane.)})
(defmethod create-element :tab [_ props _] {:node (Tab. (or (:title props) ""))})
(defmethod create-element :tool-bar [_ _ _] {:node (ToolBar.)})
(defmethod create-element :menu-bar [_ _ _] {:node (MenuBar.)})
(defmethod create-element :menu [_ props _] {:node (Menu. (or (:content props) ""))})
(defmethod create-element :menu-item [_ props _]
  (let [item (MenuItem. (or (:content props) ""))]
    (handle-action item props :click)
    {:node item}))

;; ── 基础控件 ──────────────────────────────────────────
(defmethod create-element :text [_ props _]
  (let [lbl (Label. (or (:content props) ""))
        proj-path (vnode/project-binding props)
        frame-path (vnode/frame-param-binding props)
        mk-bind (fn [path] [{:path path :apply-fn (fn [n v] (.setText ^Label n (str v)))}])]
    {:node lbl
     :bindings (when proj-path (mk-bind proj-path))
     :frame-bindings (when frame-path (mk-bind frame-path))}))

(defmethod create-element :button [_ props _]
  (let [btn (Button. (or (:content props) ""))]
    (handle-action btn props :click)
    {:node btn}))

(defmethod create-element :input [_ props f]
  (let [tf (TextField. (or (:content props) ""))
        proj-path (vnode/project-binding props)
        frame-path (vnode/frame-param-binding props)
        mk-bind (fn [path] [{:path path :apply-fn (fn [n v] (.setText ^TextField n (str v)))}])]
    (when proj-path
      (.addListener (.textProperty tf)
                    (proxy [ChangeListener] []
                      (changed [_ _ new-val] (swap! proj/project assoc-in proj-path new-val)))))
    (when frame-path
      (.addListener (.textProperty tf)
                    (proxy [ChangeListener] []
                      (changed [_ _ new-val] (swap! (frame/params-atom f) assoc-in frame-path new-val)))))
    {:node tf
     :bindings (when proj-path (mk-bind proj-path))
     :frame-bindings (when frame-path (mk-bind frame-path))}))

(defmethod create-element :text-area [_ props _]
  (let [ta (TextArea. (or (:content props) ""))
        proj-path (vnode/project-binding props)
        frame-path (vnode/frame-param-binding props)
        mk-bind (fn [path] [{:path path :apply-fn (fn [n v] (.setText ^TextArea n (str v)))}])]
    {:node ta
     :bindings (when proj-path (mk-bind proj-path))
     :frame-bindings (when frame-path (mk-bind frame-path))}))

(defmethod create-element :check-box [_ props f]
  (let [cb (CheckBox. (or (:content props) ""))
        proj-path (vnode/project-binding props)
        frame-path (vnode/frame-param-binding props)
        mk-bind (fn [path] [{:path path :apply-fn (fn [c v] (.setSelected ^CheckBox c (boolean v)))}])]
    (when-let [selected? (:checked? props)] (.setSelected cb (boolean selected?)))
    (when proj-path
      (.setOnAction cb (reify EventHandler (handle [_ _] (swap! proj/project assoc-in proj-path (.isSelected ^CheckBox cb))))))
    (when frame-path
      (.setOnAction cb (reify EventHandler (handle [_ _] (swap! (frame/params-atom f) assoc-in frame-path (.isSelected ^CheckBox cb))))))
    {:node cb
     :bindings (when proj-path (mk-bind proj-path))
     :frame-bindings (when frame-path (mk-bind frame-path))}))

(defmethod create-element :combo-box [_ props f]
  (let [cb (ComboBox.)
        proj-path (vnode/project-binding props)
        frame-path (vnode/frame-param-binding props)
        mk-bind (fn [path] [{:path path :apply-fn (fn [c v] (.setValue ^ComboBox c v))}])]
    (when-let [items (:items props)] (.addAll (.getItems cb) items))
    (when-let [val (:value props)] (.setValue cb val))
    (when proj-path
      (.setOnAction cb (reify EventHandler (handle [_ _] (swap! proj/project assoc-in proj-path (.getValue ^ComboBox cb))))))
    (when frame-path
      (.setOnAction cb (reify EventHandler (handle [_ _] (swap! (frame/params-atom f) assoc-in frame-path (.getValue ^ComboBox cb))))))
    {:node cb
     :bindings (when proj-path (mk-bind proj-path))
     :frame-bindings (when frame-path (mk-bind frame-path))}))

(defmethod create-element :slider [_ props f]
  (let [sl (Slider. (double (or (:min props) 0)) (double (or (:max props) 100)) (double (or (:value props) 50)))
        proj-path (vnode/project-binding props)
        frame-path (vnode/frame-param-binding props)
        mk-bind (fn [path] [{:path path :apply-fn (fn [s v] (.setValue ^Slider s (double v)))}])]
    (when proj-path
      (.addListener (.valueProperty sl)
                    (proxy [ChangeListener] []
                      (changed [_ _ new-val] (swap! proj/project assoc-in proj-path new-val)))))
    (when frame-path
      (.addListener (.valueProperty sl)
                    (proxy [ChangeListener] []
                      (changed [_ _ new-val] (swap! (frame/params-atom f) assoc-in frame-path new-val)))))
    {:node sl
     :bindings (when proj-path (mk-bind proj-path))
     :frame-bindings (when frame-path (mk-bind frame-path))}))

(defmethod create-element :progress [_ props _]
  {:node (ProgressBar. (double (or (:value props) 0)))})

(defmethod create-element :separator [_ _ _] {:node (Separator.)})
(defmethod create-element :image [_ props _] {:node (javafx.scene.image.ImageView. (or (:src props) ""))})

(defmethod create-element :list-view [_ props f]
  (let [lv (ListView.)
        proj-path (vnode/project-binding props)
        frame-path (vnode/frame-param-binding props)
        mk-bind (fn [path] [{:path path :apply-fn (fn [l v]
                                                    (when-let [items (:items v)]
                                                      (.clear (.getItems ^ListView l))
                                                      (.addAll (.getItems ^ListView l) items)))}])]
    (when-let [items (:items props)] (.addAll (.getItems lv) items))
    (when proj-path
      (.setOnMouseClicked lv (reify EventHandler (handle [_ _]
                                                   (when-let [selected (.. ^ListView lv getSelectionModel getSelectedItem)]
                                                     (swap! proj/project assoc-in proj-path selected))))))
    (when frame-path
      (.setOnMouseClicked lv (reify EventHandler (handle [_ _]
                                                   (when-let [selected (.. ^ListView lv getSelectionModel getSelectedItem)]
                                                     (swap! (frame/params-atom f) assoc-in frame-path selected))))))
    {:node lv
     :bindings (when proj-path (mk-bind proj-path))
     :frame-bindings (when frame-path (mk-bind frame-path))}))

(defmethod create-element :tree-view [_ _ _] {:node (TreeView.)})

(defmethod create-element :radio-button [_ props f]
  (let [rb (RadioButton. (or (:content props) ""))
        proj-path (vnode/project-binding props)
        frame-path (vnode/frame-param-binding props)
        mk-bind (fn [path] [{:path path :apply-fn (fn [r v] (.setSelected ^RadioButton r (boolean v)))}])]
    (when-let [selected? (:checked? props)] (.setSelected rb (boolean selected?)))
    (when proj-path
      (.setOnAction rb (reify EventHandler (handle [_ _] (swap! proj/project assoc-in proj-path (.isSelected ^RadioButton rb))))))
    (when frame-path
      (.setOnAction rb (reify EventHandler (handle [_ _] (swap! (frame/params-atom f) assoc-in frame-path (.isSelected ^RadioButton rb))))))
    {:node rb
     :bindings (when proj-path (mk-bind proj-path))
     :frame-bindings (when frame-path (mk-bind frame-path))}))

(defmethod create-element :hyperlink [_ props _]
  (let [hl (Hyperlink. (or (:content props) ""))]
    (handle-action hl props :click)
    {:node hl}))

(defmethod create-element :color-picker [_ _ _] {:node (ColorPicker.)})

(defmethod create-element :default [tag _ _]
  {:node (Label. (str "(unknown: " tag ")"))})