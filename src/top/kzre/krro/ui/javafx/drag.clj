(ns top.kzre.krro.ui.javafx.drag
  (:import [javafx.scene.input TransferMode ClipboardContent DragEvent MouseEvent]
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
    {:x             (.getScreenX e)
     :y             (.getScreenY e)
     :transfer-mode (transfer-mode->kw (.getTransferMode e))
     :data          (when db (.getString db))
     :accepted?     accepted?
     :source-node   (.getGestureSource e)
     :target-node   (.getSource e)}))

(defn setup-drag-source!
  [node {:keys [content-fn transfer-modes on-drag-start on-drag-end]
         :or   {transfer-modes [:move]}}]
  (let [modes-array (transfer-modes->array transfer-modes)]
    ;; 拖拽检测事件：MouseEvent
    (.setOnDragDetected node
                        (reify EventHandler
                          (handle [_  e]
                            (let [data (when content-fn (content-fn node))]
                              (when data
                                (let [db (.startDragAndDrop node modes-array)
                                      content (ClipboardContent.)]
                                  (.putString content data)
                                  (.setContent db content)))
                              (when on-drag-start
                                ;; 拖拽开始时的标准化事件（无拖拽数据）
                                (on-drag-start {:x (.getScreenX ^DragEvent e)
                                                :y (.getScreenY ^DragEvent e)
                                                :data ""
                                                :accepted? false}))
                              (.consume e)))))
    ;; 拖拽完成事件：DragEvent
    (when on-drag-end
      (.setOnDragDone node
                      (reify EventHandler
                        (handle [_ e]
                          (on-drag-end (->event-map  ^DragEvent e true))
                          (.consume e)))))))

(defn setup-drag-target!
  [node {:keys [accept-fn on-drag-over on-drag-dropped]}]
  (when on-drag-over
    (.setOnDragOver node
                    (reify EventHandler
                      (handle [_ e]
                        (let [evt (->event-map e false)
                              accepted? (if accept-fn (accept-fn node evt) true)]
                          (if accepted?
                            (.acceptTransferModes ^DragEvent  e (into-array TransferMode [TransferMode/MOVE]))
                            (.acceptTransferModes ^DragEvent  e (into-array TransferMode [])))
                          (on-drag-over node (assoc evt :accepted? accepted?))
                          (.consume e))))))
  (when on-drag-dropped
    (.setOnDragDropped node
                       (reify EventHandler
                         (handle [_  e]
                           (let [evt (->event-map ^DragEvent e false)
                                 accepted? (if accept-fn (accept-fn node evt) true)]
                             (.setDropCompleted ^DragEvent e accepted?)
                             (on-drag-dropped node (assoc evt :accepted? accepted?))
                             (.consume e)))))))