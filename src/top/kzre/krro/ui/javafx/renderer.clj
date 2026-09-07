(ns top.kzre.krro.ui.javafx.renderer
  "Krrō 内核渲染协议 (krro.core.ui) 的 JavaFX 实现。
   利用 JavaFX 节点树直接匹配窗口布局，无需额外状态。"
  (:require
   [taoensso.timbre :as log]
   [top.kzre.krro.core.frame :as frame]
   [top.kzre.krro.core.ui.protocol :as ui]
   [top.kzre.krro.core.window :as win :refer [native-object]]
   [top.kzre.krro.core.window-layout :as window-layout]
   [top.kzre.krro.ui.core.bind :as bind]
   [top.kzre.krro.ui.core.core :as krro.ui]
   [top.kzre.krro.ui.core.diff :as diff]
   [top.kzre.krro.ui.core.vnode :as vnode])
  (:import
    (java.util Arrays Collection)
   (javafx.application Platform)
   (javafx.geometry Orientation)
   (javafx.scene Node Parent)
   (javafx.scene.control SplitPane)
   (javafx.scene.layout BorderPane StackPane)))

(defonce ^:private frame-vnode-key        ::frame-vnode)
(defonce ^:private frame-bind-manager-key ::frame-bind-manager)

;; frame-id -> fx-node 的映射
(defonce ^:private frame-containers (atom {}))

(defn ensure-frame-bind-manager [f]
  (or (frame/param f frame-bind-manager-key)
      (let [m (bind/create-bind-manager (frame/params-atom f))]
        (frame/set-param! f frame-bind-manager-key m)
        m)))

(defn get-frame-bind-manager [f]
  (frame/param f frame-bind-manager-key))

(defn kw->orient [kw]
  (case kw
    :horizontal Orientation/HORIZONTAL
    :vertical Orientation/VERTICAL))


(defn- window-layout-diff!
  [window]
  (letfn
    [(diff! [layout-desc fx-node]
       (if (window-layout/leaf? layout-desc)
         ;; 叶子节点：尝试从 frame-containers 缓存获取，否则创建新的 StackPane
         (let [fid (window-layout/frame-id layout-desc)]
           (or (get @frame-containers fid)
               (let [node (StackPane.)]
                 (doto node
                   (.setMaxWidth  Double/MAX_VALUE)
                   (.setMaxHeight  Double/MAX_VALUE)
                   (.setPrefWidth Double/MAX_VALUE)
                   (.setPrefHeight Double/MAX_VALUE)
                   (.setMinWidth  0)
                   (.setMinHeight  0))
                 (swap! frame-containers assoc fid node)
                 node)))
         ;; 分割节点
         (let [[direction _props & children-desc] layout-desc
               ratios (window-layout/get-ratios layout-desc)  ; 返回 [r1 r2] 向量
               old-split (when (and fx-node (instance? SplitPane fx-node))
                           fx-node)
               new-split (let [s (or old-split (SplitPane.))
                               o (kw->orient direction)]
                           (doto s
                             (.setMaxWidth Double/MAX_VALUE)
                             (.setMaxHeight Double/MAX_VALUE)
                             (.setPrefWidth Double/MAX_VALUE)
                             (.setPrefHeight Double/MAX_VALUE)
                             (.setMinWidth 0)
                             (.setMinHeight 0))
                           (when (not= o (.getOrientation s))
                             (.setOrientation s o))
                           s)
               old-item-v (if old-split
                            (vec  (.getItems ^SplitPane old-split))
                            [])
               new-children (mapv (fn [child-layout old-child]
                                    (diff! child-layout old-child))
                                  children-desc
                                  (concat old-item-v (repeat nil)))]
           ;; 校验引用不同，并更新子项列表
           (let [items (.getItems ^SplitPane new-split)
                 current-items (vec items)]
             (when-not (= current-items new-children)
               (.clear items)
               (.addAll items ^Collection new-children)))
           ;; 设置分割位置
           (let [current-positions (.getDividerPositions ^SplitPane new-split)
                 new-positions (double-array ratios)]
             (when-not (Arrays/equals current-positions new-positions)
               (.setDividerPositions ^SplitPane new-split new-positions)))
           new-split)))]
    (let [native-win (win/native-window window)
          layout (win/layout-desc window)
          stage (native-object native-win)
          scene (.getScene stage)
          ^BorderPane root (.getRoot scene)
          content (.getCenter ^BorderPane root)
          ]
      (log/debug "Syncing window layout, layout:" layout)
      (let [new-content (diff! layout content )]
        (.setCenter ^BorderPane root new-content)
        new-content))))

(defrecord JavaFxRenderer [factory node-renderer]
  ui/IRenderer
  (render-frame [_ ui-desc frame]
    (Platform/runLater
      (fn []
        (let [win (frame/window frame)
              content (window-layout-diff! win)]
          (log/debug "Rendering frame" (frame/frame-id frame) "into content node" content)
          (ensure-frame-bind-manager frame)
          (let [frame-container (get @frame-containers (frame/frame-id frame))
                new-vnode (vnode/edn->vnode ui-desc)
                old-vnode (frame/param frame frame-vnode-key)]
            (frame/set-param! frame frame-vnode-key
                              (diff/diff! factory node-renderer frame frame-container old-vnode new-vnode)))))))

  (destroy-frame [_ frame]
    (Platform/runLater
      (fn []
        (let [frame-id (frame/frame-id frame)]
          (when-let [^Node container (get @frame-containers frame-id)]
            (log/debug "Destroying frame" (frame/frame-id frame) "- removing container")
            (let [frame-container (frame/param frame frame-vnode-key)]
              (frame/set-param! frame frame-vnode-key
                                (diff/diff! factory node-renderer frame container frame-container
                                            ;; 随便一个节点进去diff，清空原本所有的副作用
                                            (krro.ui/edn->vnode [:block {:direction :vertical}])))
              (when-let [^Parent p (.getParent container)]
                (.remove (.getChildren p) container))
              (swap! frame-containers dissoc frame-id))))))))

(defn make-renderer [factory node-renderer]
  (JavaFxRenderer. factory node-renderer))