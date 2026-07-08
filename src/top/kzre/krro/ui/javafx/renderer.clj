(ns top.kzre.krro.ui.javafx.renderer
  "Krrō 内核渲染协议 (krro.core.ui) 的 JavaFX 实现。
   使用 krro-ui-core 的 diff! 进行增量更新。"
  (:require [top.kzre.krro.core.ui.protocol :as ui]
            [top.kzre.krro.ui.core.diff :as diff]
            [top.kzre.krro.core.frame :as frame]
            [top.kzre.krro.ui.core.vnode :as vnode])
  (:import [javafx.application Platform]))


(defrecord JavaFxRenderer [root-pane factory node-renderer]
  ui/IRenderer
  (render-layout [_this el-spec f]
    (Platform/runLater
      (fn []
        (let [new-vnode (vnode/edn->vnode el-spec)
              old (frame/param f ::vnode)]
          (frame/set-param! f ::vnode (diff/diff! factory node-renderer f root-pane old new-vnode))))))
  (destroy-ui! [_this]
    (Platform/runLater
      (fn []
        (.clear (.getChildren root-pane))
        ))))