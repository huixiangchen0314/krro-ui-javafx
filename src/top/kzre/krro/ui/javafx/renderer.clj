(ns top.kzre.krro.ui.javafx.renderer
  "Krrō 内核渲染协议 (krro.core.ui) 的 JavaFX 实现。
   利用 JavaFX 节点树直接匹配窗口布局，无需额外状态。"
  (:require
    [taoensso.timbre :as log]
    [top.kzre.krro.core.frame :as frame]
    [top.kzre.krro.core.hook :as hook]
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
    (javafx.event EventHandler)
    (javafx.scene Node Parent)
    (javafx.scene.control SplitPane SplitPane$Divider)
    (javafx.scene.input MouseEvent)
    (javafx.scene.layout BorderPane StackPane)))

(defonce ^:private frame-vnode-key    ::frame-vnode)
(defonce ^:private frame-bind-ctx-key ::bind-ctx)
(def ^:private split-path-key         ::split-path)
(def ^:private attached-divider-key   ::attached-divider)
(def ^:private frame-click-handler-key ::frame-click-handler)

(def ^:private pos-eps              1e-6)

;; ── 活动 frame 高亮样式 ───────────────────────────────
(def ^:private frame-border-width 2)
(def ^:private frame-border-selected-color   "#4a90d9")
(def ^:private frame-border-unselected-color "transparent")

;; frame-id -> fx-node 的映射
(defonce ^:private frame-containers (atom {}))

(defn ensure-frame-bind-ctx [f]
  (or (frame/param f frame-bind-ctx-key)
      (let [m (bind/create (frame/params-atom f))]
        (frame/set-param! f frame-bind-ctx-key m)
        m)))

(defn get-frame-bind-ctx [f]
  (frame/param f frame-bind-ctx-key))


;; ── 活动 frame 高亮 ──────────────────────────────────

(defn- apply-frame-style!
  "给 frame 容器节点设置边框样式。
   选中/未选中使用相同 border-width，避免切换焦点时子节点布局位移。"
  [^Node node selected?]
  (.setStyle node
             (str "-fx-border-width: " frame-border-width "px;"
                  "-fx-border-style: solid;"
                  "-fx-border-color: "
                  (if selected?
                    frame-border-selected-color
                    frame-border-unselected-color)
                  ";")))

(defn- refresh-frame-styles!
  "刷新 window 中所有 frame 容器的高亮样式。
   只在 JavaFX Application Thread 调用。"
  [window]
  (let [selected-id (some-> (win/current-frame window) frame/frame-id)]
    (doseq [f (win/frames window)]
      (let [fid       (frame/frame-id f)
            ^Node node (get @frame-containers fid)]
        (when node
          (apply-frame-style! node (= fid selected-id)))))))


;; ── 点击切焦点 ───────────────────────────────────────

(defn- attach-frame-click-listener!
  "在 frame 容器的捕获阶段挂鼠标按下监听。
   点击 frame 内任意位置（包括子组件）都会把该 frame 设为 current frame。
   不 consume 事件，子组件仍能正常处理鼠标事件。
   重复调用安全：旧的 handler 会先被移除。"
  [^StackPane node window fid]
  (when-let [^EventHandler old (.get (.getProperties node) frame-click-handler-key)]
    (.removeEventFilter node MouseEvent/MOUSE_PRESSED old))
  (let [handler (reify EventHandler
                  (handle [_ _]
                    (let [current-id (some-> (win/current-frame window) frame/frame-id)]
                      (when (not= fid current-id)
                        ;; set-current-frame! 会触发 current-frame-changed-hook，
                        ;; 由 hook 负责刷新样式。
                        (win/set-current-frame! window fid)))))]
    (.put (.getProperties node) frame-click-handler-key handler)
    (.addEventFilter node MouseEvent/MOUSE_PRESSED handler)))


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

(defn- attach-divider-listeners!
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
                                (win/set-split-ratio! w p (first positions))))))))))))


;; ── 布局 diff ────────────────────────────────────────

(defn- window-layout-diff!
  [window]
  (letfn
    [(diff! [layout-desc fx-node path]
       (if (window-layout/leaf? layout-desc)
         ;; 叶子节点：尝试从 frame-containers 缓存获取，否则创建新的 StackPane
         (let [fid (window-layout/frame-id layout-desc)
               node (or (get @frame-containers fid)
                        (let [n (StackPane.)]
                          (swap! frame-containers assoc fid n)
                          n))]
           (attach-frame-click-listener! node window fid)
           node)
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
        ;; 兜底刷新：初始化、结构变化后保证样式正确
        ;; （hook 只在 current-frame 变化时触发，覆盖不到初始创建等场景）
        (refresh-frame-styles! window)
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


;; ── Hook 注册 ────────────────────────────────────────

(defonce ^:private current-frame-hook-registered? (atom false))

(defn- on-current-frame-changed
  "current-frame-changed-hook 回调。
   set-current-frame! 可能从任意线程触发（命令、脚本），
   所有 UI 操作必须调度回 JavaFX Application Thread。"
  [window _old-frame-id _new-frame-id]
  (Platform/runLater
    (fn []
      (refresh-frame-styles! window))))

(defn make-renderer [factory node-renderer]
  ;; 幂等注册：一个进程内只挂一次，避免重复调用 make-renderer 时累积 hook
  (when (compare-and-set! current-frame-hook-registered? false true)
    (hook/add-hook! :krro.core/current-frame-changed-hook
                    on-current-frame-changed))
  (JavaFxRenderer. factory node-renderer))