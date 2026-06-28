(ns top.kzre.krro.ui.javafx.diff
  "基于 EDN 的虚拟 DOM diff 算法，受 VDomDiff (C#) 启发。
   支持 key 匹配、属性 diff、事件绑定/解绑。"
  (:require [top.kzre.krro.ui.javafx.tags :as tags]
            [top.kzre.krro.core.ui.protocol :as ui])
  (:import [javafx.scene Node]
           [javafx.scene.layout Pane]))

(declare patch-node)

;; ── 公共入口 ──────────────────────────────
(defn patch-edn
  "在 parent 容器中，根据 old-edn 和 new-edn 执行增量更新。
   renderer 用于创建新元素。"
  [renderer ^Pane parent old-edn new-edn]
  (if (nil? new-edn)
    ;; 移除所有子节点
    (.clear (.getChildren parent))
    (patch-node renderer parent nil old-edn new-edn)))

;; ── 内部实现 ──────────────────────────────

(defn- effective-key
  "从 EDN 元素中提取有效 key，优先 :id 属性。"
  [edn]
  (when (and (vector? edn) (>= (count edn) 2))
    (let [attrs (second edn)]
      (when (map? attrs)
        (:id attrs)))))

(defn- node-by-key
  "在节点序列中查找具有指定 EDN 的节点索引。"
  [nodes edn key]
  (when key
    (first (keep-indexed #(when (= key (effective-key %2)) %1) nodes))))

(defn- bind-events
  "根据 EDN 属性中的 :on-command 绑定事件。"
  [^Node node attrs]
  ;; 这里复用 tags 中的 bind-command 逻辑
  (tags/bind-command node attrs))

(defn- unbind-events
  "解绑旧属性中的事件（当前实现中命令事件仅由 setOnAction 替换，无需显式解绑，但保留接口）。"
  [^Node node old-attrs]
  ;; 目前简单忽略，因为 JavaFX 按钮重新 setOnAction 会覆盖旧处理器。
  )

(defn- update-properties
  "更新节点属性，先解绑旧事件，再绑定新事件，然后调用 tags/update-attrs。"
  [^Node node old-attrs new-attrs]
  (when (and old-attrs new-attrs)
    (unbind-events node old-attrs)
    (bind-events node new-attrs)
    (tags/update-attrs node old-attrs new-attrs)))

(defn- create-element
  "创建新元素并挂载其生命周期钩子（暂无）。"
  [renderer edn]
  (let [node (ui/render-element renderer edn)]
    ;; 如果有 :on-mounted 等，可在此触发
    node))

(defn- replace-child
  "在 parent 中替换指定索引的节点，如果旧节点存在则删除。"
  [^Pane parent index old-node new-node]
  (when old-node
    (.remove (.getChildren parent) old-node))
  (when new-node
    (if index
      (.add (.getChildren parent) index new-node)
      (.add (.getChildren parent) new-node))))

(defn- patch-children
  "递归比较父容器中的新旧子 EDN 列表。"
  [renderer ^Pane parent old-children new-children]
  (let [old-cnt (count old-children)
        new-cnt (count new-children)
        children-list (.getChildren parent)
        old-nodes (into [] (take old-cnt) children-list)  ;; 假设前 old-cnt 个是旧子节点
        old-key-map (reduce (fn [m edn]
                              (if-let [k (effective-key edn)]
                                (assoc m k edn)
                                m))
                            {}
                            old-children)]
    ;; 遍历新子节点
    (loop [new-idx 0
           old-idx 0
           old-remaining (vec old-children)
           old-nodes-remaining (vec old-nodes)]
      (if (>= new-idx new-cnt)
        ;; 移除多余旧节点
        (doseq [i (range old-idx (count old-remaining))]
          (when-let [node (nth old-nodes-remaining i nil)]
            (.remove children-list node)))
        (let [new-edn (nth new-children new-idx)
              new-key (effective-key new-edn)
              matched-old-idx (when new-key
                                (node-by-key old-remaining new-edn new-key))]
          (if matched-old-idx
            ;; key 匹配
            (let [old-edn (nth old-remaining matched-old-idx)
                  old-node (nth old-nodes-remaining matched-old-idx)]
              ;; 如果匹配的节点不在当前位置，移动节点
              (when (not= matched-old-idx old-idx)
                (.remove children-list old-node)
                (.add children-list new-idx old-node))
              ;; 递归 patch
              (patch-node renderer parent old-node old-edn new-edn)
              ;; 从旧列表中移除匹配项
              (recur (inc new-idx)
                     (if (= matched-old-idx old-idx) (inc old-idx) old-idx)
                     (vec (concat (subvec old-remaining 0 matched-old-idx)
                                  (subvec old-remaining (inc matched-old-idx))))
                     (vec (concat (subvec old-nodes-remaining 0 matched-old-idx)
                                  (subvec old-nodes-remaining (inc matched-old-idx))))))
            ;; 无 key 匹配，尝试按顺序
            (if (< old-idx old-cnt)
              (let [old-edn (nth old-remaining old-idx)
                    old-node (nth old-nodes-remaining old-idx)]
                ;; 如果 key 不同，但顺序匹配，可以继续
                (if (and (not new-key) (not (effective-key old-edn)))
                  ;; 两者都没有 key，按顺序 patch
                  (do (patch-node renderer parent old-node old-edn new-edn)
                      (recur (inc new-idx) (inc old-idx)
                             (subvec old-remaining 1)
                             (subvec old-nodes-remaining 1)))
                  ;; 否则作为新节点处理
                  (let [new-node (create-element renderer new-edn)]
                    (replace-child parent new-idx nil new-node)
                    (recur (inc new-idx) old-idx old-remaining old-nodes-remaining))))
              ;; 没有旧节点可匹配，直接创建
              (let [new-node (create-element renderer new-edn)]
                (replace-child parent new-idx nil new-node)
                (recur (inc new-idx) old-idx old-remaining old-nodes-remaining)))))))))


;; ── 配合 renderer 使用的 diff 函数 ────────
(defn diff-and-update
  "使用 renderer 在根容器上 diff 新旧 EDN。"
  [renderer old-edn new-edn]
  (let [parent ^Pane (:root-pane renderer)]
    (patch-edn renderer parent old-edn new-edn)))