(ns top.kzre.krro.ui.javafx.core
  "Krrō JavaFX 入口，管理窗口、交互器、状态栏、渲染器和 Frame。"
  (:require
   [clojure.string :as str]
   [top.kzre.krro.core.core :as krro]
   [top.kzre.krro.core.frame :as frame]
   [top.kzre.krro.core.interactive :as i]
   [top.kzre.krro.core.keymap :as km]
   [top.kzre.krro.core.message :as msg]
   [top.kzre.krro.core.mode :as mode]
   [top.kzre.krro.core.project :as proj]
   [top.kzre.krro.core.ui.protocol :as ui]
   [top.kzre.krro.core.window :as win]
   [top.kzre.krro.ui.core.protocol :as proto]
   [top.kzre.krro.ui.javafx.factory :as factory]
   [top.kzre.krro.ui.javafx.patcher :as patcher]
   [top.kzre.krro.ui.javafx.plugin]
   [top.kzre.krro.ui.javafx.renderer :as renderer]
   [top.kzre.krro.ui.javafx.window])
  (:import
   (java.util Collection)
   [javafx.application Platform]
   (javafx.event EventHandler)
   [javafx.scene Scene]
   [javafx.scene.control ChoiceDialog Label TextInputDialog]
   (javafx.scene.input KeyEvent)
   [javafx.scene.layout BorderPane]
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
          init (fn [old-p p f]
                 (let [cleanup (init-fn node old-p p f)]
                   (reset! cleanup-atom cleanup)))
          update (fn [old-p p f] (init-fn node old-p p f))]
      (init nil props frame)                         ;; 首次初始化
      {:node node
       :on-update (fn [_element old-vnode new-vnode]
                    (let [old-props (proto/node-props old-vnode)
                          new-props (proto/node-props new-vnode)]
                      (when (not= (select-keys old-props watched-props)
                                  (select-keys new-props watched-props))
                        (@cleanup-atom)           ;; 清理旧运行时
                        (update old-props new-props frame)))) ;; 重新初始化
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
  (read-args [_this spec]
    (mapv (fn [item]
            (let [[type prompt & opts] (if (keyword? item)
                                         [item (str "Enter " (name item) ": ")]
                                         item)
                  prompt (or prompt "Enter value: ")]
              (case type
                :string
                (let [dialog (TextInputDialog. "")]
                  (.setTitle dialog "Input")
                  (.setHeaderText dialog prompt)
                  (let [result (.showAndWait dialog)]
                    (if (.isPresent result)
                      (.get result)
                      "")))

                :number
                (let [dialog (TextInputDialog. "")]
                  (.setTitle dialog "Number Input")
                  (.setHeaderText dialog prompt)
                  (let [result (.showAndWait dialog)]
                    (if (.isPresent result)
                      (let [text (.get result)]
                        (try
                          (Long/parseLong text)
                          (catch NumberFormatException _
                            (try
                              (Double/parseDouble text)
                              (catch NumberFormatException _
                                (msg/warn "Invalid number format, returning nil")
                                nil)))))
                      nil)))

                :keyword
                (let [dialog (TextInputDialog. "")]
                  (.setTitle dialog "Keyword Input")
                  (.setHeaderText dialog prompt)
                  (let [result (.showAndWait dialog)]
                    (if (.isPresent result)
                      (keyword (.get result))
                      nil)))

                :choice
                (let [options-fn (first opts)
                      options (if (fn? options-fn) (options-fn) options-fn)
                      choice-prompt (or (second opts) "Choose: ")
                      ^ChoiceDialog dialog
                      (ChoiceDialog. (first options) ^Collection options)]
                  (.setTitle dialog "Choice")
                  (.setHeaderText dialog choice-prompt)
                  (let [result (.showAndWait dialog)]
                    (if (.isPresent result)
                      (.get result)
                      nil)))

                ;; 未知类型
                (do
                  (msg/error (str "Unsupported interactive spec: " type))
                  nil))))
          spec)))


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
  "创建并显示 Krrō 主舞台。返回 {:keys [stage]}。
   Scene 的根节点是一个空的 BorderPane，其底部固定为状态栏，
   中心区域由渲染器在同步布局描述时动态填充。"
  [& {:keys [width height title]
      :or {width 1024 height 768 title "Krrō"}}]
  (let [root (BorderPane.)
        scene (Scene. root (double width) (double height))
        stage (Stage.)]
    ;(.add (.getStylesheets scene) "stylesheets/main.css")
    (.setOnKeyPressed scene
                      (reify EventHandler
                        (handle [_ e]
                          (let [^KeyEvent ke e]
                            (when-not (.isConsumed ke)
                              (let [key-desc (key-event->key-desc ke)
                                    keymaps (mode/keymaps (win/active-frame))]
                                (try
                                  (km/handle-key! key-desc keymaps)
                                  (catch Exception ex
                                    (.printStackTrace ex)))))))))
    ;; 将状态栏固定在底部
    (.setBottom root (build-status-bar))
    ;; 中心区域不预先创建，由渲染器接管
    (.setScene stage scene)
    (.setTitle stage title)
    (.show stage)
    {:stage stage}))

;; ── 启动辅助 ────────────────────────────────────────────
(defn launch-app
  "启动 JavaFX 并执行初始化回调。
   回调签名为 (fn [window])，window 是 IWindow 实例。
   内部已创建渲染器、设置交互器、初始化项目。"
  [init-fn]
  (Platform/startup
    (fn []
      (proj/init-project!)
      (i/set-interactor! (->JavaFxInteractor))
      (let [{:keys [stage]} (create-stage)
            krro-renderer (renderer/make-renderer (factory/->JavaFxElementFactory)
                                                  (patcher/->JavaFxNodePatcher))]
        ;; 先设置渲染器，再创建窗口（窗口创建时会自动激活 fundamental 模式，此时渲染器已就绪）
        (ui/set-renderer! krro-renderer)
        (let [window (krro/create-window! stage)
              initial-frame (win/current-frame window)]
          ;; 保留兼容 TODO 移除
          (alter-var-root #'frame/*current-frame* (constantly initial-frame))
          ;; 调用外部回调，只传递 window
          (init-fn window))))))