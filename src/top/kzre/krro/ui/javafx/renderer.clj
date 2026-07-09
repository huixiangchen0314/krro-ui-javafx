(ns top.kzre.krro.ui.javafx.renderer
  "Krrō 内核渲染协议 (krro.core.ui) 的 JavaFX 实现。
   使用 krro-ui-core 的 diff! 进行增量更新。"
  (:require [top.kzre.krro.core.ui.protocol :as ui]
            [top.kzre.krro.ui.core.diff :as diff]
            [top.kzre.krro.core.frame :as frame]
            [top.kzre.krro.ui.core.vnode :as vnode]
            [top.kzre.krro.ui.core.bind :as bind])
  (:import [javafx.application Platform]))

(defonce frame-bind-manager-key ::frame-bind-manager)
(def ^:private frame-vnode-key ::vnode)
(defn ensure-frame-bind-manager [f]
  (or (frame/param f frame-bind-manager-key)
      (let [m (bind/create-bind-manager (frame/params-atom f))]
        (frame/set-param! f frame-bind-manager-key m)
        m)))

(defrecord JavaFxRenderer [root-pane factory node-renderer]
  ui/IRenderer
  (render-layout [_this el-spec f]
    (Platform/runLater
      (fn []
        (ensure-frame-bind-manager f)   ;; 确保管理器存在
        (let [new-vnode (vnode/edn->vnode el-spec)
              old (frame/param f frame-vnode-key)]
          (frame/set-param! f frame-vnode-key (diff/diff! factory node-renderer f root-pane old new-vnode))))))
  (destroy-ui! [_this]
    (Platform/runLater
      (fn []
        (.clear (.getChildren root-pane))
        ;; 清理逻辑暂不处理管理器，由 Frame 生命周期管理
        ))))