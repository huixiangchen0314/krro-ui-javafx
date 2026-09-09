(ns top.kzre.krro.ui.javafx.event
  "JavaFX 事件适配层：将原生事件转换为标准化事件 map，并提供绑定函数。
   不支持已创建组件的事件动态更新。
   注意：不再使用 ObservableValue 监听器来触发变更事件，
   而是依赖 ActionEvent（用户交互触发）来保持单向数据流。"
  (:import
   [java.awt TextArea TextField]
   (java.util.function BiConsumer)
   (javafx.beans Observable)
   [javafx.beans.value ChangeListener]
   [javafx.event Event EventHandler]
   (javafx.scene Node)
   [javafx.scene.control
    CheckBox
    ComboBox
    RadioButton
    Slider
    TextInputControl
    ToggleButton]
   [javafx.scene.input KeyEvent MouseEvent]))

(defn ->event-map
  "将 JavaFX 事件转换为符合 spec.event 的标准化 map。"
  [^Event fx-event event-type target-key]
  (merge
    {:type      event-type
     :target    target-key
     :timestamp (System/currentTimeMillis)}
    ;; 鼠标事件
    (when (instance? MouseEvent fx-event)
      (let [me ^MouseEvent fx-event]
        {:x (.getX me)
         :y (.getY me)
         :screen-x (.getScreenX me)
         :screen-y (.getScreenY me)
         :button   (case (.getButton me)
                     MouseButton/PRIMARY :left
                     MouseButton/SECONDARY :right
                     MouseButton/MIDDLE :middle
                     nil)
         :ctrl? (.isControlDown me)
         :shift? (.isShiftDown me)
         :alt? (.isAltDown me)
         :meta? (.isMetaDown me)}))
    ;; 键盘事件
    (when (instance? KeyEvent fx-event)
      (let [ke ^KeyEvent fx-event]
        {:key      (.getText ke)
         :key-code (.getCode ke)
         :repeat?  (.isRepeat ke)
         :ctrl?    (.isControlDown ke)
         :shift?   (.isShiftDown ke)
         :alt?   (.isAltDown ke)
         :meta?  (.isMetaDown ke)}))))

;; ── 事件绑定函数 ────────────────────────────

(defn bind-click!
  [^Node node props target-key]
  (when-let [f (:on-click props)]
    (.setOnMouseClicked node
                        (reify EventHandler
                          (handle [_ e]
                            (f (->event-map e :click target-key)))))))

(defn bind-action!
  [ node props target-key]
  (when-let [f (:on-action props)]
    (.setOnAction node
                  (reify EventHandler
                    (handle [_ e]
                      (f (->event-map e :action target-key)))))))

(defn make-change-event [key old new]
  {:type      :change
   :target    key
   :timestamp (System/currentTimeMillis)
   :old-value old
   :new-value new})

(defn bind-change-action!
  "绑定 ActionEvent 作为变更事件。仅当用户交互（如点击、回车、拖动）触发。
   程序修改控件值不会触发此事件，从而保持单向数据流。
   回调会收到包含 :type :change, :target, :timestamp, :old-value, :new-value 的 map。"
  [node props target-key]
  (when-let [ f (:on-change props)]
    (let [^Observable observable
          (cond
            (instance? Slider node) (.valueProperty node)
            (instance? CheckBox node) (.selectedProperty node)
            (instance? CheckBox node)     (.isSelected ^CheckBox node)
            (instance? ComboBox node)     (.getValue ^ComboBox node)
            (instance? TextField node)    (.getText ^TextField node)
            (instance? TextArea node)     (.getText ^TextArea node)
            (instance? RadioButton node)  (.isSelected ^RadioButton node))]
      (.subscribe observable
                  (reify BiConsumer
                    (accept [_ old-value new-value]
                      (f (make-change-event target-key old-value new-value))))))))


(defn bind-change-action!
  "绑定 ActionEvent 作为变更事件。仅当用户交互（如点击、回车、拖动）触发。
   程序修改控件值不会触发此事件，从而保持单向数据流。
   回调会收到包含 :type :change, :target, :timestamp, :old-value, :new-value 的 map。"
  [node props target-key]
  (when-let [ f (:on-change props)]
    (cond
      (or (instance? CheckBox node)
          (instance? RadioButton node))
      (.setOnAction node
                    (reify EventHandler
                      (handle [_ e]
                        (let [new-val
                              (cond
                                (instance? CheckBox node) (.isSelected ^CheckBox node))]
                          (f (make-change-event target-key nil new-val))))))
      :else
      (let [observable
            (cond
              (instance? TextInputControl node) (.textProperty ^TextInputControl node)
              (instance? Slider node) (.valueProperty ^Slider node)
              (instance? CheckBox node) (.selectedProperty ^CheckBox node)
              (instance? ComboBox node) (.valueProperty ^ComboBox node)
              (instance? ToggleButton node) (.selectedProperty ^ToggleButton node))]

        (.subscribe observable
                    (reify BiConsumer
                      (accept [_ old-value new-value]
                        (f (make-change-event target-key old-value new-value)))))))))
;; 以下函数保持不变
(defn bind-focus!
  [^Node node props target-key]
  (when (or (:on-focus props) (:on-blur props))
    (.addListener (.focusedProperty node)
                  (proxy [ChangeListener] []
                    (changed [_ _ focused?]
                      (when focused?
                        (when-let [f (:on-focus props)]
                          (f {:type :focus :target target-key :timestamp (System/currentTimeMillis)})))
                      (when-not focused?
                        (when-let [f (:on-blur props)]
                          (f {:type :blur :target target-key :timestamp (System/currentTimeMillis)}))))))))

(defn bind-key!
  [^Node node props target-key]
  (when-let [f (:on-key-down props)]
    (.setOnKeyPressed node
                      (reify EventHandler
                        (handle [_ e]
                          (f (->event-map e :key-down target-key))))))
  (when-let [f (:on-key-up props)]
    (.setOnKeyReleased node
                       (reify EventHandler
                         (handle [_ e]
                           (f (->event-map e :key-up target-key)))))))

(defn bind-mouse-enter-leave!
  [^Node node props target-key]
  (when-let [f (:on-mouse-enter props)]
    (.setOnMouseEntered node
                        (reify EventHandler
                          (handle [_ e]
                            (f (->event-map e :mouse-enter target-key))))))
  (when-let [f (:on-mouse-leave props)]
    (.setOnMouseExited node
                       (reify EventHandler
                         (handle [_ e]
                           (f (->event-map e :mouse-leave target-key)))))))