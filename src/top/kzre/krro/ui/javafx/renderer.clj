(ns top.kzre.krro.ui.javafx.renderer
  "Krrō JavaFX 渲染器核心，支持基于 EDN 的增量更新。"
  (:require [top.kzre.krro.core.ui.protocol :as ui]
            [top.kzre.krro.ui.javafx.diff :as diff]
            [top.kzre.krro.ui.javafx.tags :as tags])
  (:import (javafx.application Platform)))

(defrecord JavaFxRenderer [root-pane edn-atom]   ;; edn-atom 是 (atom nil)
  ui/IRenderer
  (render-element [this element]
    (tags/render-tag element this))
  (render-layout [this root-element]
    (Platform/runLater
      (fn []
        (let [old-edn @edn-atom]
          (if old-edn
            (do
              (diff/diff-and-update this old-edn root-element)
              (reset! edn-atom root-element))
            (do
              (.clear (.getChildren root-pane))
              (when-let [node (ui/render-element this root-element)]
                (.add (.getChildren root-pane) node))
              (reset! edn-atom root-element)))))))
  (destroy-ui! [_]
    (Platform/runLater
      (fn []
        (.clear (.getChildren root-pane))
        (reset! edn-atom nil)))))

;; 在 tags.clj 中新增 update-attrs 函数