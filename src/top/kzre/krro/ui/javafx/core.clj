(ns top.kzre.krro.ui.javafx.core
  "Krrō JavaFX 入口，管理窗口、交互器、状态栏、渲染器和 Frame。"
  (:require
    [clojure.string :as str]
    [top.kzre.krro.core.frame :as frame]
    [top.kzre.krro.core.interactive :as i]
    [top.kzre.krro.core.message :as msg]
    [top.kzre.krro.core.mode :as mode]
    [top.kzre.krro.core.project :as proj]
    [top.kzre.krro.core.ui.protocol :as ui]
    [top.kzre.krro.ui.core.protocol :as proto]
    [top.kzre.krro.ui.javafx.impl :as impl]
    [top.kzre.krro.ui.javafx.plugin]
    [top.kzre.krro.core.keymap :as km]
    [top.kzre.krro.ui.javafx.renderer :as renderer])
  (:import
    [javafx.application Platform]
    (javafx.event EventHandler)
    [javafx.scene Scene]
    [javafx.scene.control ChoiceDialog Label TextInputDialog]
    (javafx.scene.input KeyCode KeyEvent)
    [javafx.scene.layout BorderPane VBox]
    [javafx.stage Stage]))


(defn make-component
  "创建一个组件工厂函数。
   watched-props - 需要监听的 props 关键字向量，变化时触发 update
   create-fn     - 无参函数，返回组件的根节点（仅首次调用）
   init-fn       - (fn [node props frame] ...) 返回清理函数，每次 props 变化时调用。
                  首次挂载时也会调用，用于初始化组件内部运行时。
   返回一个组件函数，可注册为虚拟 DOM 标签。"
  [watched-props create-fn init-fn]
  (fn [props frame]
    (let [node (create-fn)                       ;; 创建根节点（仅一次）
          cleanup-atom (atom (fn []))
          init (fn [p f]
                 (let [cleanup (init-fn node p f)]
                   (reset! cleanup-atom cleanup)))]
      (init props frame)                         ;; 首次初始化
      {:node node
       :on-update (fn [_element old-vnode new-vnode]
                    (let [old-props (proto/node-props old-vnode)
                          new-props (proto/node-props new-vnode)]
                      (when (not= (select-keys old-props watched-props)
                                  (select-keys new-props watched-props))
                        (@cleanup-atom)           ;; 清理旧运行时
                        (init new-props frame)))) ;; 重新初始化
       :on-unmount (fn [_ _] (@cleanup-atom))})))

(defn- key-event->key-desc
  "将 JavaFX KeyEvent 转换为 Emacs 风格的键描述字符串，例如 'C-x', 'C-S-d', 'RET'。"
  [^KeyEvent e]
  (let [code (.getCode e)
        modifiers (set
                    (cond-> []
                            (.isControlDown e) (conj "C")
                            (.isShiftDown e)   (conj "S")
                            (.isAltDown e)     (conj "M")
                            (.isMetaDown e)    (conj "s"))) ; Windows Key / Command
        modifier-str (if (seq modifiers)
                       (str (str/join "-" (sort modifiers)) "-")
                       "")
        key-name (case code
                   KeyCode/ENTER "RET"
                   KeyCode/SPACE "SPC"
                   KeyCode/TAB "TAB"
                   KeyCode/ESCAPE "ESC"
                   KeyCode/BACK_SPACE "DEL"
                   KeyCode/DELETE "DEL"
                   KeyCode/UP "UP"
                   KeyCode/DOWN "DOWN"
                   KeyCode/LEFT "LEFT"
                   KeyCode/RIGHT "RIGHT"
                   KeyCode/F1 "f1"
                   KeyCode/F2 "f2"
                   KeyCode/F3 "f3"
                   KeyCode/F4 "f4"
                   KeyCode/F5 "f5"
                   KeyCode/F6 "f6"
                   KeyCode/F7 "f7"
                   KeyCode/F8 "f8"
                   KeyCode/F9 "f9"
                   KeyCode/F10 "f10"
                   KeyCode/F11 "f11"
                   KeyCode/F12 "f12"
                   (let [ch (str/lower-case (.getText e))]
                     (if (and ch (not (str/blank? ch)))
                       ch
                       (.getName code))))]
    (keyword (str modifier-str key-name))))

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
  "创建并显示 Krrō 主舞台。返回 {:keys [stage root-pane]}。"
  [& {:keys [width height title]
      :or {width 1024 height 768 title "Krrō"}}]
  (let [root (BorderPane.)
        center-pane (VBox.)
        scene (Scene. root (double width) (double height))
        stage (Stage.)]
    (.setOnKeyPressed scene
                      (reify EventHandler
                        (handle [_ e]
                          (let [^KeyEvent ke e] ;; 类型提示，避免反射
                            (when-not (.isConsumed ke)
                              (let [key-desc (key-event->key-desc ke)
                                    keymaps (mode/keymaps frame/*current-frame*)]
                                (try
                                  (km/handle-key! key-desc keymaps)
                                  (catch Exception ex
                                    (.printStackTrace ex)))))))))
    (.setCenter root center-pane)
    (.setBottom root (build-status-bar))
    (.setScene stage scene)
    (.setTitle stage title)

    (.show stage)
    {:stage stage :root-pane center-pane}))

;; ── 启动辅助 ────────────────────────────────────────────
(defn launch-app
  "启动 JavaFX 并执行初始化回调。
   回调接收 factory, renderer, root-pane 以及当前 Frame。"
  [init-fn]
  (Platform/startup
    (fn []
      (proj/init-project!)
      (i/set-interactor! (->JavaFxInteractor))
      (let [{:keys [root-pane]} (create-stage)
            factory (impl/->JavaFxElementFactory)
            node-renderer (impl/->JavaFxNodeRenderer)
            f (frame/create-frame :id :main)]
        ;; 设置全局当前 Frame
        (alter-var-root #'frame/*current-frame* (constantly f))
        ;; 创建渲染器（初始无旧 VNode）
        (let [krro-renderer (renderer/->JavaFxRenderer root-pane factory node-renderer)]
          (ui/set-renderer! krro-renderer)
          (init-fn factory krro-renderer root-pane f))))))