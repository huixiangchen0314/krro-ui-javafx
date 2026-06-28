(ns top.kzre.krro.ui.javafx.renderer
  "Krrō 内核渲染协议 (krro.core.ui) 的 JavaFX 实现。
   使用 krro-ui-core 的 diff! 进行增量更新。"
  (:require [top.kzre.krro.core.ui.protocol :as ui]
            [top.kzre.krro.ui.core.diff :as diff]
            [top.kzre.krro.ui.core.vnode :as vnode])
  (:import [javafx.application Platform]))

(defrecord JavaFxRenderer [root-pane factory node-renderer old-vnode]
  ui/IRenderer
  (render-element [this element]
    (when-let [vnode (vnode/edn->vnode element)]
      (diff/diff! factory node-renderer root-pane @old-vnode vnode)))
  (render-layout [this root-element]
    (Platform/runLater
      (fn []
        (let [new-vnode (vnode/edn->vnode root-element)
              old @old-vnode]
          (reset! old-vnode (diff/diff! factory node-renderer root-pane old new-vnode))))))
  (destroy-ui! [this]
    (Platform/runLater
      (fn []
        (.clear (.getChildren root-pane))
        (reset! old-vnode nil)))))