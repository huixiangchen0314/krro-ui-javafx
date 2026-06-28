(ns top.kzre.krro.ui.javafx.renderer
  "Krrō 内核渲染协议 (krro.core.ui) 的 JavaFX 实现。
   内部使用 krro-ui-core 抽象：VNode、IElementFactory、IRenderer（节点操作）。
   递归挂载整棵 VNode 树，确保子节点正确添加。"
  (:require [top.kzre.krro.core.ui.protocol :as ui]
            [top.kzre.krro.ui.core.protocol :as proto]
            [top.kzre.krro.ui.core.vnode :as vnode])
  (:import (javafx.application Platform)))

(defn- mount-vnode
  "递归挂载 vnode 及其子树，返回创建的平台元素。"
  [factory node-renderer vnode]
  (when vnode
    (let [el (proto/create-element factory vnode)]
      ;; 挂载子节点
      (doseq [child (proto/node-children vnode)]
        (when-let [child-el (mount-vnode factory node-renderer child)]
          (proto/append-child node-renderer el child-el)))
      el)))

(defrecord JavaFxRenderer [root-pane factory node-renderer]
  ui/IRenderer
  (render-element [this element]
    (when-let [vnode (vnode/edn->vnode element)]
      (mount-vnode factory node-renderer vnode)))
  (render-layout [this root-element]
    (Platform/runLater
      (fn []
        (.clear (.getChildren root-pane))
        (when-let [vnode (vnode/edn->vnode root-element)]
          (when-let [root-el (mount-vnode factory node-renderer vnode)]
            (.add (.getChildren root-pane) root-el))))))
  (destroy-ui! [this]
    (Platform/runLater
      (fn [] (.clear (.getChildren root-pane))))))