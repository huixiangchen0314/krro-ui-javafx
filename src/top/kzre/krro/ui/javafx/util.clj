(ns top.kzre.krro.ui.javafx.util
  (:import
   (javafx.geometry Orientation Pos)))


(defn kw->orient [kw]
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