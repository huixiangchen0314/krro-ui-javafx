(ns top.kzre.krro.ui.javafx.plugin
  "提供 :krro.plugin/javafx-tag 插件类型，允许外部插件注册自定义 UI 标签。"
  (:require [top.kzre.krro.core.plugin :as plugin :refer [defplugin]]
            [top.kzre.krro.ui.javafx.tags :as tags]))

(defplugin :krro.plugin/javafx-tag [tag handler]
                      "注册一个新的 JavaFX UI 标签。
                       tag 为标签关键字，handler 为 (fn [props context] -> javafx.scene.Node)。
                       示例:
                       (plugin/register-plugin!
                         {:name :my-plugin/widget
                          :type :krro.plugin/javafx-tag
                          :tag :my-widget
                          :handler (fn [props ] ...)})"
                      (defmethod tags/create-element tag [_ props]
                        (handler props)))