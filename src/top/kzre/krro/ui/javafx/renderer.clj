(ns top.kzre.krro.ui.javafx.renderer
  "Krrō 内核渲染协议 (krro.core.ui) 的 JavaFX 实现。
   利用 JavaFX 节点树直接匹配窗口布局，无需额外状态。"
  (:require [top.kzre.krro.core.frame :as frame]
            [top.kzre.krro.core.ui.protocol :as ui]
            [top.kzre.krro.core.window :as win :refer [native-object]]
            [top.kzre.krro.ui.core.bind :as bind]
            [top.kzre.krro.ui.core.diff :as diff]
            [top.kzre.krro.ui.core.vnode :as vnode]
            [taoensso.timbre :as log])
  (:import (java.util Collection)
           (javafx.application Platform)
           (javafx.geometry Orientation)
           (javafx.scene Parent Scene)
           (javafx.scene.control SplitPane)
           (javafx.scene.layout BorderPane StackPane)))

(defonce ^:private frame-vnode-key        ::frame-vnode)
(defonce ^:private frame-container-key    ::frame-container)
(defonce frame-bind-manager-key ::frame-bind-manager)

(defn ensure-frame-bind-manager [f]
  (or (frame/param f frame-bind-manager-key)
      (let [m (bind/create-bind-manager (frame/params-atom f))]
        (frame/set-param! f frame-bind-manager-key m)
        m)))

(defn- build-layout-node
  [layout-desc window frame-id->container]
  (if (win/leaf? layout-desc)
    (let [fid (first layout-desc)
          container (StackPane.)]
      (log/debug "Build leaf container for frame" fid)
      (swap! frame-id->container assoc fid container)
      container)
    (let [[direction props & children] layout-desc
          ratios (:ratios props (repeat (count children) (/ 1.0 (count children))))
          split (SplitPane.)
          _ (.setOrientation split (case direction
                                     (:horizontal :left :right) Orientation/HORIZONTAL
                                     (:vertical :up :down) Orientation/VERTICAL))
          child-nodes (mapv #(build-layout-node % window frame-id->container) children)]
      (.addAll (.getItems split) ^Collection child-nodes)
      (when (> (count ratios) 1)
        (.setDividerPositions split (double-array (butlast ratios))))
      split)))

(defn- content-root
  [node]
  (if (instance? BorderPane node)
    (.getCenter ^BorderPane node)
    node))

(defn- set-content-root!
  [scene root new-content]
  (if (instance? BorderPane root)
    (do
      (log/debug "Setting BorderPane center to new content")
      (.setCenter ^BorderPane root new-content))
    (do
      (log/debug "Setting new root on scene")
      (.setRoot scene new-content))))

(defn- node-matches-layout?
  [node layout]
  (let [content (content-root node)]
    (cond
      (win/leaf? layout)
      (instance? StackPane content)
      (instance? SplitPane content)
      (let [direction (first layout)
            expected-orient (case direction
                              (:horizontal :left :right) Orientation/HORIZONTAL
                              (:vertical :up :down) Orientation/VERTICAL)
            children-nodes (vec (.getItems ^SplitPane content))
            children-layout (drop 2 layout)]
        (and (= expected-orient (.getOrientation ^SplitPane content))
             (= (count children-nodes) (count children-layout))
             (every? true? (map node-matches-layout? children-nodes children-layout))))
      :else false)))

(defn- sync-window-layout!
  [window]
  (let [native-win (win/native-window window)
        stage (native-object native-win)
        scene (.getScene stage)
        root (when scene (.getRoot scene))
        layout (win/layout-desc window)
        content (when root (content-root root))]
    (log/debug "Syncing window layout, root:" (type root) ", layout:" layout)
    (when-not (and content (node-matches-layout? root layout))
      (log/info "Layout mismatch, rebuilding UI tree")
      (let [frame-id->container (atom {})
            new-content (build-layout-node layout window frame-id->container)]
        (if scene
          (set-content-root! scene root new-content)
          (let [new-scene (Scene. new-content)]
            (.setScene stage new-scene)))
        (doseq [f (win/frames window)]
          (let [fid (frame/frame-id f)
                container (get @frame-id->container fid)]
            (log/debug "Associating container for frame" fid)
            (frame/set-param! f frame-container-key container)))))))

(defn- get-frame-container
  [window frame]
  (or (frame/param frame frame-container-key)
      (do
        (log/debug "Container not found for frame" (frame/frame-id frame) "- syncing window layout")
        (sync-window-layout! window)
        (let [container (frame/param frame frame-container-key)]
          (log/debug "After sync, container:" container)
          container))))

(defrecord JavaFxRenderer [factory node-renderer]
  ui/IRenderer
  (render-frame [_this ui-desc frame]
    (Platform/runLater
      (fn []
        (let [win (frame/window frame)]
          (when win
            (let [container (get-frame-container win frame)]
              (log/debug "Rendering frame" (frame/frame-id frame) "into container" container)
              (ensure-frame-bind-manager frame)
              (let [new-vnode (vnode/edn->vnode ui-desc)
                    old-vnode (frame/param frame frame-vnode-key)]
                (frame/set-param! frame frame-vnode-key
                                  (diff/diff! factory node-renderer frame container old-vnode new-vnode)))))))))

  (destroy-frame [_this frame]
    (Platform/runLater
      (fn []
        (when-let [container (frame/param frame frame-container-key)]
          (log/debug "Destroying frame" (frame/frame-id frame) "- removing container")
          (when-let [parent (.getParent container)]
            (cond
              (instance? SplitPane parent)
              (.remove (.getItems ^SplitPane parent) container)
              (instance? Parent parent)
              (.remove (.getChildrenUnmodifiable ^Parent parent) container)))
          (frame/remove-param! frame frame-container-key)
          (frame/remove-param! frame frame-vnode-key)
          (frame/remove-param! frame frame-bind-manager-key))))))

(defn make-renderer [factory node-renderer]
  (JavaFxRenderer. factory node-renderer))