(ns top.kzre.krro.ui.javafx.drag
  "JavaFX 拖拽交互实现，支持 drag-source 和 drag-target。"
  (:import (javafx.scene Node)
           [javafx.scene.input TransferMode ClipboardContent DragEvent]
           [javafx.event EventHandler]))

(defn- transfer-mode->kw [^TransferMode mode]
  (cond (= mode TransferMode/MOVE) :move
        (= mode TransferMode/COPY) :copy
        (= mode TransferMode/LINK) :link
        :else :unknown))

(defn- transfer-modes->array [modes]
  (into-array TransferMode
              (mapv #(case %
                       :move TransferMode/MOVE
                       :copy TransferMode/COPY
                       :link TransferMode/LINK)
                    modes)))

(defn- ->event-map
  "从 DragEvent 构建标准化事件 map（用于拖拽中/后回调）。"
  [^DragEvent e accepted?]
  (let [db (.getDragboard e)]
    {:x             (.getX e)
     :y             (.getY e)
     :transfer-mode (transfer-mode->kw (.getTransferMode e))
     :data          (when db (.getString db))
     :accepted?     accepted?
     :source-node   (.getGestureSource e)
     :target-node   (.getSource e)}))

(defn setup-drag-source!
  "为节点设置拖拽源。"
  [node {:keys [content-fn transfer-modes on-drag-start on-drag-end]
         :or   {transfer-modes [:move]}}]
  (let [modes-array (transfer-modes->array transfer-modes)]
    (.setOnDragDetected node
                        (reify EventHandler
                          (handle [_ e]
                            (let [data (when content-fn (content-fn node))]
                              (when data
                                (let [db (.startDragAndDrop node modes-array)
                                      content (ClipboardContent.)]
                                  (.putString content data)
                                  (.setContent db content)))
                              (when on-drag-start
                                (on-drag-start {:x (.getX ^DragEvent e)
                                                :y (.getY ^DragEvent e)
                                                :data ""
                                                :accepted? false}))
                              (.consume e)))))
    (.setOnDragDone node
                    (when on-drag-end
                      (reify EventHandler
                        (handle [_ e]
                          (on-drag-end (->event-map ^DragEvent e true))
                          (.consume e)))))))

(defn clear-drag-source!
  "清除节点的拖拽源监听器。"
  [^Node node]
  (.setOnDragDetected node nil)
  (.setOnDragDone node nil))

(defn setup-drag-target!
  "为节点设置拖拽目标。"
  [node {:keys [accept-fn on-drag-over on-drag-dropped]}]
  (.setOnDragOver node
                  (when on-drag-over
                    (reify EventHandler
                      (handle [_ e]
                        (let [evt (->event-map e false)
                              accepted? (if accept-fn (accept-fn node evt) true)]
                          (if accepted?
                            (.acceptTransferModes ^DragEvent e (into-array TransferMode [TransferMode/MOVE]))
                            (.acceptTransferModes ^DragEvent e (into-array TransferMode [])))
                          (on-drag-over node (assoc evt :accepted? accepted?))
                          (.consume e))))))
  (.setOnDragDropped node
                     (when on-drag-dropped
                       (reify EventHandler
                         (handle [_ e]
                           (let [evt (->event-map e false)
                                 accepted? (if accept-fn (accept-fn node evt) true)]
                             (.setDropCompleted ^DragEvent e accepted?)
                             (on-drag-dropped node (assoc evt :accepted? accepted?))
                             (.consume e)))))))

(defn clear-drag-target!
  "清除节点的拖拽目标监听器。"
  [^Node node]
  (.setOnDragOver node nil)
  (.setOnDragDropped node nil))