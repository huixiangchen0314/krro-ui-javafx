(ns top.kzre.krro.ui.javafx.core
  "Krrō JavaFX 库入口。提供主窗口骨架、交互器、渲染器等。
   窗口采用单一中心区域替换，模式 EDN 包含菜单栏、工具栏和内容。"
  (:require [top.kzre.krro.core.interactive :as i]
            [top.kzre.krro.core.message :as msg]
            [top.kzre.krro.core.ui.protocol :as ui]
            [top.kzre.krro.ui.javafx.renderer :as r])
  (:import (javafx.application Platform)
           (javafx.scene Scene)
           (javafx.scene.control ChoiceDialog Label TextInputDialog)
           (javafx.scene.layout BorderPane VBox)
           (javafx.stage Stage)))

;; ── 交互器 ─────────────────────────────────
(defrecord JavaFxInteractor []
  i/IInteractor
  (read-text [this prompt]
    (let [dialog (TextInputDialog.)]
      (.setTitle dialog "Input")
      (.setHeaderText dialog prompt)
      (let [result (.showAndWait dialog)]
        (if result (or result "") ""))))
  (read-number [this prompt]
    (let [input (i/read-text this prompt)]
      (try (Long/parseLong input) (catch Exception _ 0))))
  (read-choice [this prompt options]
    (let [dialog (ChoiceDialog. (first options) options)]
      (.setTitle dialog "Choice")
      (.setHeaderText dialog prompt)
      (let [result (.showAndWait dialog)]
        (if result result (first options))))))

;; ── 状态栏 ─────────────────────────────────
(defn- build-status-bar []
  (let [status-label (Label. "Ready")]
    (add-watch msg/messages :status-bar
               (fn [_ _ _ new-msgs]
                 (when-let [m (last new-msgs)]
                   (Platform/runLater
                     (fn []
                       (.setText status-label (str "[" (name (:type m)) "] " (:content m))))))))
    status-label))

;; ── 主舞台创建 ──────────────────────────────
(defn create-stage
  [& {:keys [width height title]
      :or {width 1024 height 768 title "Krrō"}}]
  (let [root (BorderPane.)
        ;; 中心区域容器，由模式布局完全替换
        center-pane (VBox.)
        renderer (r/->JavaFxRenderer center-pane (atom nil))
        scene (Scene. root (double width) (double height))
        stage (Stage.)]
    (ui/set-renderer! renderer)
    (.setCenter root center-pane)
    ;; 状态栏固定在底部
    (.setBottom root (build-status-bar))
    (.setScene stage scene)
    (.setTitle stage title)
    (.show stage)
    stage))

;; ── JavaFX 启动辅助 ─────────────────────────
(defn launch-app
  "启动 JavaFX 并执行给定的初始化回调。"
  [init-fn]
  (Platform/startup (fn [] (init-fn))))