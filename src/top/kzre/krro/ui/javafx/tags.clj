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

(defmulti render-tag (fn [tag _props _context] tag))

(defn- common-bind-text [node path]
  (bind/register! node path (fn [n v]
                              (if (instance? Label n)
                                (.setText ^Label n (str v))
                                (.setText ^TextField n (str v))))))

;; ── 布局容器（只创建空容器）───────────────────────────
(defmethod render-tag :block [_ props _]
  (if (= (:direction props :vertical) :vertical) (VBox.) (HBox.)))

(defmethod render-tag :split-pane [_ _ _] (SplitPane.))
(defmethod render-tag :scroll [_ _ _] (ScrollPane.))
(defmethod render-tag :tab-panel [_ _ _] (TabPane.))
(defmethod render-tag :tab [_ props _] (Tab. (or (:title props) "")))
(defmethod render-tag :tool-bar [_ _ _] (ToolBar.))
(defmethod render-tag :menu-bar [_ _ _] (MenuBar.))
(defmethod render-tag :menu [_ props _] (Menu. (or (:content props) "")))

(defmethod render-tag :menu-item [_ props {:keys [execute-command]}]
  (let [item (MenuItem. (or (:content props) ""))]
    (when-let [cmd-id (get-in props [:on :click])]
      (.setOnAction item (reify EventHandler (handle [_ _] (execute-command cmd-id)))))
    item))

;; ── 基础控件 ──────────────────────────────────────────
(defmethod render-tag :text [_ props _]
  (let [lbl (Label. (or (:content props) ""))]
    (when-let [path (:bind props)] (common-bind-text lbl path))
    lbl))

(defmethod render-tag :button [_ props {:keys [execute-command]}]
  (let [btn (Button. (or (:content props) ""))]
    (when-let [cmd-id (get-in props [:on :click])]
      (.setOnAction btn (reify EventHandler (handle [_ _] (execute-command cmd-id)))))
    btn))

(defmethod render-tag :input [_ props {:keys [execute-command]}]
  (let [tf (TextField. (or (:content props) ""))]
    (when-let [path (:bind props)] (common-bind-text tf path))
    (when-let [cmd-id (get-in props [:on :change])]
      (.addListener (.textProperty tf) (proxy [ChangeListener] [] (changed [_ _ _ new-val] (execute-command cmd-id new-val)))))
    tf))

(defmethod render-tag :text-area [_ props _]
  (let [ta (TextArea. (or (:content props) ""))]
    (when-let [path (:bind props)] (common-bind-text ta path))
    ta))

(defmethod render-tag :check-box [_ props {:keys [execute-command]}]
  (let [cb (CheckBox. (or (:content props) ""))]
    (when-let [selected? (:checked? props)] (.setSelected cb (boolean selected?)))
    (when-let [path (:bind props)] (bind/register! cb path (fn [^CheckBox c v] (.setSelected c (boolean v)))))
    (when-let [cmd-id (get-in props [:on :click])]
      (.setOnAction cb (reify EventHandler (handle [_ _] (execute-command cmd-id)))))
    cb))

(defmethod render-tag :combo-box [_ props {:keys [execute-command]}]
  (let [cb (ComboBox.)]
    (when-let [items (:items props)] (.addAll (.getItems cb) items))
    (when-let [val (:value props)] (.setValue cb val))
    (when-let [path (:bind props)] (bind/register! cb path (fn [^ComboBox c v] (.setValue c v))))
    (when-let [cmd-id (get-in props [:on :click])]
      (.setOnAction cb (reify EventHandler (handle [_ _] (execute-command cmd-id)))))
    cb))

(defmethod render-tag :slider [_ props {:keys [execute-command]}]
  (let [sl (Slider. (double (or (:min props) 0)) (double (or (:max props) 100)) (double (or (:value props) 50)))]
    (when-let [path (:bind props)] (bind/register! sl path (fn [^Slider s v] (.setValue s (double v)))))
    (when-let [cmd-id (get-in props [:on :change])]
      (.addListener (.valueProperty sl) (proxy [ChangeListener] [] (changed [_ _ _ new-val] (execute-command cmd-id new-val)))))
    sl))

(defmethod render-tag :progress [_ props _] (ProgressBar. (double (or (:value props) 0))))
(defmethod render-tag :separator [_ _ _] (Separator.))
(defmethod render-tag :image [_ props _] (javafx.scene.image.ImageView. (or (:src props) "")))

(defmethod render-tag :list-view [_ props {:keys [execute-command]}]
  (let [lv (ListView.)]
    (when-let [items (:items props)] (.addAll (.getItems lv) items))
    (when-let [path (:bind props)] (bind/register! lv path (fn [^ListView l v] (when-let [items (:items v)] (.clear (.getItems l)) (.addAll (.getItems l) items)))))
    (when-let [cmd-id (get-in props [:on :click])]
      (.setOnAction lv (reify EventHandler (handle [_ _] (execute-command cmd-id)))))
    lv))

(defmethod render-tag :tree-view [_ _ _] (TreeView.))

(defmethod render-tag :radio-button [_ props {:keys [execute-command]}]
  (let [rb (RadioButton. (or (:content props) ""))]
    (when-let [selected? (:checked? props)] (.setSelected rb (boolean selected?)))
    (when-let [path (:bind props)] (bind/register! rb path (fn [^RadioButton r v] (.setSelected r (boolean v)))))
    (when-let [cmd-id (get-in props [:on :click])]
      (.setOnAction rb (reify EventHandler (handle [_ _] (execute-command cmd-id)))))
    rb))

(defmethod render-tag :hyperlink [_ props {:keys [execute-command]}]
  (let [hl (Hyperlink. (or (:content props) ""))]
    (when-let [cmd-id (get-in props [:on :click])]
      (.setOnAction hl (reify EventHandler (handle [_ _] (execute-command cmd-id)))))
    hl))

(defmethod render-tag :color-picker [_ _ _] (ColorPicker.))

(defmethod render-tag :default [tag _ _]
  (Label. (str "(unknown: " tag ")")))