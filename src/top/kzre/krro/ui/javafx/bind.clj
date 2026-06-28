(ns top.kzre.krro.ui.javafx.bind
  "响应式数据绑定核心。
   不直接引用 JavaFX 控件类，避免在加载命名空间时触发 Toolkit 初始化。
   所有 UI 更新自动在 JavaFX 线程执行。"
  (:require [top.kzre.krro.core.project :as proj])
  (:import [javafx.application Platform]
           [javafx.scene Node]))

(def ^:private label-class "javafx.scene.control.Label")
(def ^:private text-field-class "javafx.scene.control.TextField")
(def ^:private text-area-class "javafx.scene.control.TextArea")
(def ^:private check-box-class "javafx.scene.control.CheckBox")
(def ^:private combo-box-class "javafx.scene.control.ComboBox")
(def ^:private slider-class "javafx.scene.control.Slider")

(defn- class-name [node]
  (.getName (class node)))

(defmulti update-control-value (fn [node val] (class-name node)))

(defmethod update-control-value label-class [node val]
  (.setText ^javafx.scene.control.Labeled node (str val)))

(defmethod update-control-value text-field-class [node val]
  (.setText ^javafx.scene.control.TextInputControl node (str val)))

(defmethod update-control-value text-area-class [node val]
  (.setText ^javafx.scene.control.TextInputControl node (str val)))

(defmethod update-control-value check-box-class [node val]
  (.setSelected ^javafx.scene.control.CheckBox node (boolean val)))

(defmethod update-control-value combo-box-class [node val]
  (.setValue ^javafx.scene.control.ComboBoxBase node val))

(defmethod update-control-value slider-class [node val]
  (.setValue ^javafx.scene.control.Slider node (double val)))

(defmethod update-control-value :default [_ _] nil)

(defn- run-on-fx [f]
  (if (Platform/isFxApplicationThread)
    (f)
    (Platform/runLater f)))

(defn register-binding
  "为控件 node 注册数据绑定，监听项目原子中 bind-path 的变化并更新控件。"
  [^Node node bind-path]
  (when-let [current-val (get-in @proj/project bind-path)]
    (run-on-fx #(update-control-value node current-val)))
  (add-watch proj/project (keyword (str "bind-" (gensym)))
             (fn [_ _ old new]
               (let [old-val (get-in old bind-path)
                     new-val (get-in new bind-path)]
                 (when (not= old-val new-val)
                   (run-on-fx #(update-control-value node new-val)))))))