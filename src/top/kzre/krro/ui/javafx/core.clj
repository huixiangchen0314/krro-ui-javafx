(ns top.kzre.krro.ui.javafx.core
  "Krrō JavaFX 入口，管理窗口、交互器、状态栏和渲染器安装。"
  (:require [top.kzre.krro.core.interactive :as i]
            [top.kzre.krro.core.message :as msg]
            [top.kzre.krro.core.ui.protocol :as ui]
            [top.kzre.krro.ui.javafx.impl :as impl]
            [top.kzre.krro.ui.javafx.renderer :as renderer])
  (:import (javafx.application Platform)
           (javafx.scene Scene)
           (javafx.scene.control ChoiceDialog Label TextInputDialog)
           (javafx.scene.layout BorderPane VBox)
           (javafx.stage Stage)))

;; ── 交互器实现 ───────────────────────────────────────────
(defrecord JavaFxInteractor []
  i/IInteractor
  (read-text [_ prompt]
    (let [dialog (TextInputDialog.)]
      (.setTitle dialog "Input")
      (.setHeaderText dialog prompt)
      (let [result (.showAndWait dialog)]
        (if result (or result "") ""))))
  (read-number [_ prompt]
    (let [input (i/read-text (->JavaFxInteractor) prompt)]
      (try (Long/parseLong input) (catch Exception _ 0))))
  (read-choice [_ prompt options]
    (let [dialog (ChoiceDialog. (first options) options)]
      (.setTitle dialog "Choice")
      (.setHeaderText dialog prompt)
      (let [result (.showAndWait dialog)]
        (if result result (first options))))))

;; ── 状态栏 ──────────────────────────────────────────────
(defn- build-status-bar []
  (let [status-label (Label. "Ready")]
    (add-watch msg/messages :status-bar
               (fn [_ _ _ new-msgs]
                 (when-let [m (last new-msgs)]
                   (Platform/runLater
                     (fn []
                       (.setText status-label (str "[" (name (:type m)) "] " (:content m))))))))
    status-label))

;; ── 主舞台创建 ──────────────────────────────────────────
(defn create-stage
  [& {:keys [width height title]
      :or {width 1024 height 768 title "Krrō"}}]
  (let [root (BorderPane.)
        center-pane (VBox.)
        scene (Scene. root (double width) (double height))
        stage (Stage.)]
    (.setCenter root center-pane)
    (.setBottom root (build-status-bar))
    (.setScene stage scene)
    (.setTitle stage title)
    (.show stage)
    {:stage stage :root-pane center-pane}))

;; ── 启动辅助 ────────────────────────────────────────────
(defn launch-app
  "启动 JavaFX 并执行初始化回调。"
  [init-fn]
  (Platform/startup
    (fn []
      (let [{:keys [root-pane]} (create-stage)
            factory (impl/->JavaFxElementFactory)
            node-renderer (impl/->JavaFxRenderer root-pane)
            krro-renderer (renderer/->JavaFxRenderer root-pane factory node-renderer)]
        (ui/set-renderer! krro-renderer)    ;; 安装到内核，模式系统将使用此渲染器
        (init-fn factory krro-renderer root-pane)))))