(ns top.kzre.krro.ui.javafx.util
  (:import
    [javafx.animation PauseTransition]
    (javafx.event EventHandler)
    (javafx.geometry Orientation Pos)
    [javafx.util Duration]))


(defn kw->orientation [kw]
  (case kw
    :horizontal Orientation/HORIZONTAL
    :vertical Orientation/VERTICAL))

(defn kw->pos [kw]
  (case kw
    :top-left Pos/TOP_LEFT
    :top-center Pos/TOP_CENTER
    :top-right Pos/TOP_RIGHT
    :center-left Pos/CENTER_LEFT
    :center Pos/CENTER
    :center-right Pos/CENTER_RIGHT
    :bottom-left Pos/BOTTOM_LEFT
    :bottom-center Pos/BOTTOM_CENTER
    :bottom-right Pos/BOTTOM_RIGHT
    :baseline-left Pos/BASELINE_LEFT
    :baseline-center Pos/BASELINE_CENTER
    :baseline-right Pos/BASELINE_RIGHT
    Pos/CENTER)) ; 默认居中


(defn debounced
  "返回防抖回调：delay-ms 内的多次调用只执行最后一次。
   必须在 JavaFX 应用线程创建与调用。

   参数：
     - callback: (fn [value] ...) 实际执行的函数
     - delay-ms: 延迟毫秒数
   返回：
     - (fn [value] ...) 防抖包装后的回调"
  [callback delay-ms]
  (let [pending    (atom nil)
        transition (PauseTransition. (Duration/millis delay-ms))]
    (.setOnFinished transition
                    (reify EventHandler
                      (handle [_ _]
                        (when-let [v @pending]
                          (reset! pending nil)
                          (callback v)))))
    (fn [value]
      (reset! pending value)
      (.playFromStart transition))))

;; ── 节流 ─────────────────────────────────────────
(defn throttled
  "返回节流回调：每 interval-ms 至多执行一次，保证首帧立即执行，
   并在 interval 窗口结束后触发最后一次（尾部保证）。

   参数：
     - callback:    (fn [value] ...) 实际执行的函数
     - interval-ms: 节流间隔（毫秒）
   返回：
     - (fn [value] ...) 节流包装后的回调"
  [callback interval-ms]
  (let [last-fire  (atom 0)
        pending    (atom nil)
        transition (PauseTransition. (Duration/millis interval-ms))]
    (.setOnFinished transition
                    (reify EventHandler
                      (handle [_ _]
                        (when-let [v @pending]
                          (reset! pending nil)
                          (reset! last-fire (System/currentTimeMillis))
                          (callback v)))))
    (fn [value]
      (let [now (System/currentTimeMillis)]
        (if (>= (- now @last-fire) interval-ms)
          ;; 窗口已过，立即执行
          (do
            (reset! last-fire now)
            (callback value))
          ;; 窗口内，记录最后一次，等待尾部触发
          (do
            (reset! pending value)
            (.playFromStart transition)))))))