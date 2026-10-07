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
    [top.kzre.krro.ui.core.vnode :as vnode]
    [top.kzre.krro.ui.javafx.util :as javafx.util]
    [top.kzre.krro.core.core :as krro])
  (:import
    (java.util Collection)
    (javafx.application Platform)
    (javafx.beans.value ChangeListener)
    (javafx.scene Node Parent)
    (javafx.scene.control SplitPane SplitPane$Divider)
    (javafx.scene.layout BorderPane StackPane)))

(defonce ^:private frame-vnode-key    ::frame-vnode)
(defonce ^:private frame-bind-ctx-key ::bind-ctx)
(def ^:private split-path-key         ::split-path)
(def ^:private attached-divider-key   ::attached-divider)

(def ^:private pos-eps              1e-6)

;; frame-id -> fx-node 的映射
(defonce ^:private frame-containers (atom {}))

(defn ensure-frame-bind-ctx [f]
  (or (frame/param f frame-bind-ctx-key)
      (let [m (bind/create (frame/params-atom f))]
        (frame/set-param! f frame-bind-ctx-key m)
        m)))

(defn get-frame-bind-ctx [f]
  (frame/param f frame-bind-ctx-key))


;; ── ratio 同步辅助 ────────────────────────────────────

(defn- positions-equal?
  "比较 JavaFX 当前 dividerPositions 和期望位置（两叉时长度为 1）。"
  [^doubles current desired]
  (let [n (alength current)
        m (count desired)]
    (and (= n m)
         (every? (fn [i]
                   (< (Math/abs (double
                                  (- (aget current i)
                                     (double (nth desired i)))))
                      pos-eps))
                 (range n)))))

(defn- invalidate-aabbs
  "清掉树上所有 :aabb，强制下次 frame-aabb 重算。"
  [layout]
  (window-layout/prewalk
    layout
    (fn [node]
      (if (window-layout/split? node)
        (update node 1 dissoc :aabb)
        node))))

(defn- window-set-ratio!
  "把用户拖动后的第一个子节点占比写回 layout。
   path 是 split 节点在 layout-desc 中的路径，其 props 位于 (conj path 1)。"
  [window path r]
  (when (and (number? r) (< 0 r 1))
    (swap! (:layout-atom window)
           (fn [layout]
             (-> layout
                 (update-in (conj path 1) assoc :ratio (double r))
                 invalidate-aabbs)))))

(defn- attach-divider-listeners!
  "给 SplitPane 的 divider 挂位置监听。
   SplitPane$Divider 不继承 Node，没有 getProperties()，所以标记存在 SplitPane 自身。
   二叉树只有一个 divider；当 items 变化时 JavaFX 会重建 divider 实例，
   用 identical? 判断实例是否换过，换过才重挂，避免重复挂监听。"
  [^SplitPane sp window path]
  (.put (.getProperties sp) split-path-key [window path])
  (when-let [^SplitPane$Divider d (first (.getDividers sp))]
    (let [attached ^SplitPane$Divider (.get (.getProperties sp) attached-divider-key)]
      (when-not (identical? d attached)
        (.put (.getProperties sp) attached-divider-key d)
        (.addListener (.positionProperty d)
                      (reify ChangeListener
                        (changed [_ _ _ _]
                          (when-let [[w p] (.get (.getProperties sp) split-path-key)]
                            (let [positions (vec (.getDividerPositions sp))]
                              (when (seq positions)
                                ;; 两叉时只有一个 divider，其位置就是第一个子节点占比
                                (window-set-ratio! w p (first positions))))))))))))


;; ── 布局 diff ────────────────────────────────────────

(defn- window-layout-diff!
  [window]
  (letfn
    [(diff! [layout-desc fx-node path]
       (if (window-layout/leaf? layout-desc)
         ;; 叶子节点：尝试从 frame-containers 缓存获取，否则创建新的 StackPane
         (let [fid (window-layout/frame-id layout-desc)]
           (or (get @frame-containers fid)
               (let [node (StackPane.)]
                 (swap! frame-containers assoc fid node)
                 node)))
         ;; 分割节点
         (let [[direction _props & children-desc] layout-desc
               old-split (when (and fx-node (instance? SplitPane fx-node))
                           fx-node)
               new-split (let [s (or old-split (SplitPane.))
                               o (javafx.util/kw->orientation direction)]
                           (when (not= o (.getOrientation s))
                             (.setOrientation s o))
                           s)
               old-item-v (if old-split
                            (vec (.getItems ^SplitPane old-split))
                            [])
               new-children (mapv (fn [child-layout old-child i]
                                    (diff! child-layout old-child
                                           (conj path (+ 2 i))))
                                  children-desc
                                  (krro/take-padded (count children-desc) old-item-v)
                                  (range))]
           ;; 先更新子项列表，这会重建 dividers
           (let [items (.getItems ^SplitPane new-split)
                 current-items (vec items)]
             (when-not (= current-items new-children)
               (.clear items)
               (.addAll items ^Collection new-children)))
           ;; 再挂监听（此时 dividers 已就绪）
           (attach-divider-listeners! new-split window path)
           ;; 数据 -> 视图：值不同才写，避免把用户拖动弹回去
           (let [r       (window-layout/get-ratio layout-desc)
                 desired [(double r)]
                 current (.getDividerPositions new-split)]
             (when-not (positions-equal? current desired)
               (.setDividerPositions new-split (double-array desired))))
           new-split)))]
    (let [native-win (win/native-window window)
          layout (win/layout-desc window)
          stage (native-object native-win)
          scene (.getScene stage)
          ^BorderPane root (.getRoot scene)
          content (.getCenter ^BorderPane root)]
      (log/debug "Syncing window layout, layout:" layout)
      (let [new-content (diff! layout content [])]
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
          (ensure-frame-bind-ctx frame)
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