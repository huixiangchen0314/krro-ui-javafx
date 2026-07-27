(ns top.kzre.krro.ui.javafx.window
  "将 INativeWindow 协议实现到 javafx.stage.Stage 上。"
  (:require [top.kzre.krro.core.window :refer [INativeWindow]])
  (:import [javafx.stage Stage]))

(extend-protocol INativeWindow
  Stage
  (native-show! [s] (.show s))
  (native-hide! [s] (.hide s))
  (native-close! [s] (.close s))
  (native-title [s] (.getTitle s))
  (native-set-title! [s title] (.setTitle s title))
  (native-set-bounds! [s {:keys [x y width height]}]
    (when x (.setX s (double x)))
    (when y (.setY s (double y)))
    (when width (.setWidth s (double width)))
    (when height (.setHeight s (double height))))
  (native-bounds [s]
    {:x (.getX s) :y (.getY s) :width (.getWidth s) :height (.getHeight s)})
  (native-visible? [s] (.isShowing s))
  (native-focused? [s] (.isFocused s))
  (native-object [s] s))