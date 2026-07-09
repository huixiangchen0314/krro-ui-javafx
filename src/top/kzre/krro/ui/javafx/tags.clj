(ns top.kzre.krro.ui.javafx.tags
  "JavaFX 标签多方法。每个标签只创建对应的平台节点，绑定数据与事件。
   输入控件自动实现双向绑定（直接 swap!），按钮等支持统一的 :on :click 格式或 :on-click 简写。
   签名：[tag props frame]。"
  (:require [top.kzre.krro.ui.core.bind :as bind]
            [top.kzre.krro.core.project :as proj]
            [top.kzre.krro.core.command :as cmd]
            [top.kzre.krro.ui.core.vnode :as vnode])
  (:import [javafx.beans.value ChangeListener]
           [javafx.event EventHandler]
           [javafx.scene.control Button CheckBox ComboBox ColorPicker Hyperlink Label ListView
                                 Menu MenuBar MenuItem ProgressBar RadioButton ScrollPane Separator Slider
                                 SplitPane Tab TabPane TextArea TextField ToolBar TreeView]
           [javafx.scene.layout GridPane HBox VBox]))

(defmulti create-element (fn [tag _props _frame] tag))

(defn- common-bind-text [node path]
  (bind/register! node path (fn [n v]
                              (if (instance? Label n)
                                (.setText ^Label n (str v))
                                (.setText ^TextField n (str v))))))

(defn- bind-checkbox [^CheckBox cb path]
  (bind/register! cb path (fn [c v] (.setSelected c (boolean v)))))

(defn- bind-combobox [^ComboBox cb path]
  (bind/register! cb path (fn [c v] (.setValue c v))))

(defn- bind-slider [^Slider sl path]
  (bind/register! sl path (fn [s v] (.setValue s (double v)))))

(defn- bind-listview [^ListView lv path]
  (bind/register! lv path (fn [l v] (when-let [items (:items v)]
                                      (.clear (.getItems l))
                                      (.addAll (.getItems l) items)))))

;; ── 统一事件处理 ──────────────────────────────────────
(defn- handle-action
  "为节点注册指定事件的回调。事件回调从 props 中通过 vnode/event 统一获取。
   event-key 为事件关键字（如 :click），props 为属性 map。
   回调可以是函数（直接调用）或命令关键字（通过 execute-command! 执行）。"
  [node props event-key]
  (when-let [on-spec (vnode/event props event-key)]
    (.setOnAction node
                  (reify EventHandler
                    (handle [_ _]
                      (if (fn? on-spec)
                        (on-spec node)
                        (cmd/execute-command! on-spec)))))))

;; ── 布局容器 ──────────────────────────────────────────
(defmethod create-element :block [_ props _]
  (if (= (:direction props :vertical) :vertical) (VBox.) (HBox.)))

(defmethod create-element :split-pane [_ _ _] (SplitPane.))
(defmethod create-element :scroll [_ _ _] (ScrollPane.))
(defmethod create-element :tab-panel [_ _ _] (TabPane.))
(defmethod create-element :tab [_ props _] (Tab. (or (:title props) "")))
(defmethod create-element :tool-bar [_ _ _] (ToolBar.))
(defmethod create-element :menu-bar [_ _ _] (MenuBar.))
(defmethod create-element :menu [_ props _] (Menu. (or (:content props) "")))

(defmethod create-element :menu-item [_ props _]
  (let [item (MenuItem. (or (:content props) ""))]
    (handle-action item props :click)
    item))

;; ── 基础控件 ──────────────────────────────────────────
(defmethod create-element :text [_ props _]
  (let [lbl (Label. (or (:content props) ""))]
    (when-let [path (:bind props)] (common-bind-text lbl path))
    lbl))

(defmethod create-element :button [_ props _]
  (let [btn (Button. (or (:content props) ""))]
    (handle-action btn props :click)
    btn))

(defmethod create-element :input [_ props _]
  (let [tf (TextField. (or (:content props) ""))]
    (when-let [path (:bind props)]
      (common-bind-text tf path)
      (.addListener (.textProperty tf)
                    (proxy [ChangeListener] []
                      (changed [_ _ new-val]
                        (swap! proj/project assoc-in path new-val)))))
    tf))

(defmethod create-element :text-area [_ props _]
  (let [ta (TextArea. (or (:content props) ""))]
    (when-let [path (:bind props)] (common-bind-text ta path))
    ta))

(defmethod create-element :check-box [_ props _]
  (let [cb (CheckBox. (or (:content props) ""))]
    (when-let [selected? (:checked? props)] (.setSelected cb (boolean selected?)))
    (when-let [path (:bind props)]
      (bind-checkbox cb path)
      ;; 选中状态变化时更新项目数据
      (.setOnAction cb
                    (reify EventHandler
                      (handle [_ _]
                        (swap! proj/project assoc-in path (.isSelected ^CheckBox cb))))))
    cb))

(defmethod create-element :combo-box [_ props _]
  (let [cb (ComboBox.)]
    (when-let [items (:items props)] (.addAll (.getItems cb) items))
    (when-let [val (:value props)] (.setValue cb val))
    (when-let [path (:bind props)]
      (bind-combobox cb path)
      (.setOnAction cb
                    (reify EventHandler
                      (handle [_ _]
                        (swap! proj/project assoc-in path (.getValue ^ComboBox cb))))))
    cb))

(defmethod create-element :slider [_ props _]
  (let [sl (Slider. (double (or (:min props) 0)) (double (or (:max props) 100)) (double (or (:value props) 50)))]
    (when-let [path (:bind props)]
      (bind-slider sl path)
      (.addListener (.valueProperty sl)
                    (proxy [ChangeListener] []
                      (changed [_ _ new-val]
                        (swap! proj/project assoc-in path new-val)))))
    sl))

(defmethod create-element :progress [_ props _] (ProgressBar. (double (or (:value props) 0))))
(defmethod create-element :separator [_ _ _] (Separator.))
(defmethod create-element :image [_ props _] (javafx.scene.image.ImageView. (or (:src props) "")))

(defmethod create-element :list-view [_ props _]
  (let [lv (ListView.)]
    (when-let [items (:items props)] (.addAll (.getItems lv) items))
    (when-let [path (:bind props)]
      (bind-listview lv path)
      (.setOnMouseClicked lv
                          (reify EventHandler
                            (handle [_ _]
                              (when-let [selected (.. ^ListView lv getSelectionModel getSelectedItem)]
                                (swap! proj/project assoc-in path selected))))))
    lv))

(defmethod create-element :tree-view [_ _ _] (TreeView.))

(defmethod create-element :radio-button [_ props _]
  (let [rb (RadioButton. (or (:content props) ""))]
    (when-let [selected? (:checked? props)] (.setSelected rb (boolean selected?)))
    (when-let [path (:bind props)]
      (bind/register! rb path (fn [r v] (.setSelected r (boolean v))))
      (.setOnAction rb
                    (reify EventHandler
                      (handle [_ _]
                        (swap! proj/project assoc-in path (.isSelected ^RadioButton rb))))))
    rb))

(defmethod create-element :hyperlink [_ props _]
  (let [hl (Hyperlink. (or (:content props) ""))]
    (handle-action hl props :click)
    hl))

(defmethod create-element :color-picker [_ _ _] (ColorPicker.))

(defmethod create-element :default [tag _ _]
  (Label. (str "(unknown: " tag ")")))