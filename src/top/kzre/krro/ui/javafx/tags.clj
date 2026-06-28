(ns top.kzre.krro.ui.javafx.tags
  "JavaFX 标签多方法。每个标签只创建对应的平台节点，绑定数据与事件。
   不处理子节点挂载（由 diff 引擎负责）。"
  (:require [top.kzre.krro.ui.core.bind :as bind])
  (:import [javafx.beans.value ChangeListener]
           [javafx.event EventHandler]
           [javafx.scene.control Button CheckBox ComboBox ColorPicker Hyperlink Label ListView
                                 Menu MenuBar MenuItem ProgressBar RadioButton ScrollPane Separator Slider
                                 SplitPane Tab TabPane TextArea TextField ToolBar TreeView]
           [javafx.scene.layout GridPane HBox VBox]))

(defmulti create-element (fn [tag _props _context] tag))

(defn- common-bind-text [node path]
  (bind/register! node path (fn [n v]
                              (if (instance? Label n)
                                (.setText ^Label n (str v))
                                (.setText ^TextField n (str v))))))

(defn- auto-change-command
  "为控件添加默认的 :change 事件，触发路径更新命令。
   如果 props 已有 :on :change，则不覆盖。"
  [node props {:keys [execute-command]}]
  (when (and (:bind props) (not (get-in props [:on :change])))
    (let [bind-path (:bind props)]
      (cond
        (instance? TextField node)
        (.addListener (.textProperty node)
                      (proxy [ChangeListener] []
                        (changed [_ _ new-val]
                          (execute-command :krro.command/update-path bind-path new-val))))
        (instance? Slider node)
        (.addListener (.valueProperty node)
                      (proxy [ChangeListener] []
                        (changed [_ _ new-val]
                          (execute-command :krro.command/update-path bind-path new-val))))
        (instance? CheckBox node)
        (.setOnAction node
                      (reify EventHandler
                        (handle [_ _]
                          (execute-command :krro.command/update-path bind-path (.isSelected node)))))
        (instance? ComboBox node)
        (.setOnAction node
                      (reify EventHandler
                        (handle [_ _]
                          (execute-command :krro.command/update-path bind-path (.getValue node)))))
        ;; 其他控件可扩展
        ))))

;; ── 布局容器（只创建空容器）───────────────────────────
(defmethod create-element :block [_ props _]
  (if (= (:direction props :vertical) :vertical) (VBox.) (HBox.)))

(defmethod create-element :split-pane [_ _ _] (SplitPane.))
(defmethod create-element :scroll [_ _ _] (ScrollPane.))
(defmethod create-element :tab-panel [_ _ _] (TabPane.))
(defmethod create-element :tab [_ props _] (Tab. (or (:title props) "")))
(defmethod create-element :tool-bar [_ _ _] (ToolBar.))
(defmethod create-element :menu-bar [_ _ _] (MenuBar.))
(defmethod create-element :menu [_ props _] (Menu. (or (:content props) "")))

(defmethod create-element :menu-item [_ props {:keys [execute-command]}]
  (let [item (MenuItem. (or (:content props) ""))]
    (when-let [cmd-id (get-in props [:on :click])]
      (.setOnAction item (reify EventHandler (handle [_ _] (execute-command cmd-id)))))
    item))

;; ── 基础控件 ──────────────────────────────────────────
(defmethod create-element :text [_ props _]
  (let [lbl (Label. (or (:content props) ""))]
    (when-let [path (:bind props)]
      (common-bind-text lbl path))
    lbl))

(defmethod create-element :button [_ props {:keys [execute-command]}]
  (let [btn (Button. (or (:content props) ""))]
    (when-let [cmd-id (get-in props [:on :click])]
      (.setOnAction btn (reify EventHandler (handle [_ _] (execute-command cmd-id)))))
    btn))

(defmethod create-element :input [_ props context]
  (let [tf (TextField. (or (:content props) ""))]
    (when-let [path (:bind props)] (common-bind-text tf path))
    (if-let [cmd-id (get-in props [:on :change])]
      (.addListener (.textProperty tf) (proxy [ChangeListener] [] (changed [_ _ _ new-val] ((:execute-command context) cmd-id new-val))))
      (auto-change-command tf props context))
    tf))

(defmethod create-element :text-area [_ props _]
  (let [ta (TextArea. (or (:content props) ""))]
    (when-let [path (:bind props)] (common-bind-text ta path))
    ta))

(defmethod create-element :check-box [_ props context]
  (let [cb (CheckBox. (or (:content props) ""))]
    (when-let [selected? (:checked? props)] (.setSelected cb (boolean selected?)))
    (when-let [path (:bind props)] (bind/register! cb path (fn [^CheckBox c v] (.setSelected c (boolean v)))))
    (if-let [cmd-id (get-in props [:on :click])]
      (.setOnAction cb (reify EventHandler (handle [_ _] ((:execute-command context) cmd-id))))
      (auto-change-command cb props context))
    cb))

(defmethod create-element :combo-box [_ props context]
  (let [cb (ComboBox.)]
    (when-let [items (:items props)] (.addAll (.getItems cb) items))
    (when-let [val (:value props)] (.setValue cb val))
    (when-let [path (:bind props)] (bind/register! cb path (fn [^ComboBox c v] (.setValue c v))))
    (if-let [cmd-id (get-in props [:on :click])]
      (.setOnAction cb (reify EventHandler (handle [_ _] ((:execute-command context) cmd-id))))
      (auto-change-command cb props context))
    cb))

(defmethod create-element :slider [_ props context]
  (let [sl (Slider. (double (or (:min props) 0)) (double (or (:max props) 100)) (double (or (:value props) 50)))]
    (when-let [path (:bind props)] (bind/register! sl path (fn [^Slider s v] (.setValue s (double v)))))
    (if-let [cmd-id (get-in props [:on :change])]
      (.addListener (.valueProperty sl) (proxy [ChangeListener] [] (changed [_ _ _ new-val] ((:execute-command context) cmd-id new-val))))
      (auto-change-command sl props context))
    sl))

(defmethod create-element :progress [_ props _] (ProgressBar. (double (or (:value props) 0))))
(defmethod create-element :separator [_ _ _] (Separator.))
(defmethod create-element :image [_ props _] (javafx.scene.image.ImageView. (or (:src props) "")))

(defmethod create-element :list-view [_ props {:keys [execute-command]}]
  (let [lv (ListView.)]
    (when-let [items (:items props)] (.addAll (.getItems lv) items))
    (when-let [path (:bind props)] (bind/register! lv path (fn [^ListView l v] (when-let [items (:items v)] (.clear (.getItems l)) (.addAll (.getItems l) items)))))
    (when-let [cmd-id (get-in props [:on :click])]
      (.setOnAction lv (reify EventHandler (handle [_ _] (execute-command cmd-id)))))
    lv))

(defmethod create-element :tree-view [_ _ _] (TreeView.))

(defmethod create-element :radio-button [_ props {:keys [execute-command]}]
  (let [rb (RadioButton. (or (:content props) ""))]
    (when-let [selected? (:checked? props)] (.setSelected rb (boolean selected?)))
    (when-let [path (:bind props)] (bind/register! rb path (fn [^RadioButton r v] (.setSelected r (boolean v)))))
    (when-let [cmd-id (get-in props [:on :click])]
      (.setOnAction rb (reify EventHandler (handle [_ _] (execute-command cmd-id)))))
    rb))

(defmethod create-element :hyperlink [_ props {:keys [execute-command]}]
  (let [hl (Hyperlink. (or (:content props) ""))]
    (when-let [cmd-id (get-in props [:on :click])]
      (.setOnAction hl (reify EventHandler (handle [_ _] (execute-command cmd-id)))))
    hl))

(defmethod create-element :color-picker [_ _ _] (ColorPicker.))

(defmethod create-element :default [tag _ _]
  (Label. (str "(unknown: " tag ")")))