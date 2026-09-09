(ns top.kzre.krro.ui.javafx.tags
  "JavaFX 标签多方法。每个标签只创建对应的平台节点，返回节点和可选的绑定描述。
   数据绑定通过 :bindings 和 :frame-bindings 声明，由工厂统一注册。
   事件处理委托给 top.kzre.krro.ui.javafx.event，遵循平台无关的事件规范。
   保留对 Krrō 命令系统的支持：若事件属性值为关键字，则自动作为命令 ID 执行。
   双向绑定通过 getter（单向同步）和 setter/on-change（用户操作写回）实现，
   不再使用 ObservableValue 自动监听，确保单向数据流。"
  (:require [top.kzre.krro.core.core :as krro]
            [top.kzre.krro.ui.javafx.event :as event]
            [top.kzre.krro.ui.javafx.util :as javafx.util])
  (:import (java.util Collection)
           (javafx.scene.control Button CheckBox ComboBox Hyperlink Label Menu MenuBar MenuItem ProgressBar RadioButton ScrollPane Separator Slider
                                 SplitPane TextArea TextField ToolBar)
           (javafx.scene.image ImageView)
           (javafx.scene.layout HBox VBox)))

(defmulti create-element (fn [tag _props _frame] tag))

(defn- target-key [props]
  (or (:key props) (:id props) "unknown"))

(defn- resolve-action
  "将事件属性值转换为函数。若已是函数则直接返回；若为关键字则视为命令 ID 并返回执行该命令的函数；
   若为 [command-id & args] 向量，则返回带参数执行该命令的函数；否则返回 nil。"
  [on-spec]
  (cond
    (fn? on-spec)
    on-spec

    (keyword? on-spec)
    (fn [_] (krro/exe-command! on-spec))

    (and (vector? on-spec) (keyword? (first on-spec)))
    (let [cmd-id (first on-spec)
          args   (rest on-spec)]
      (fn [_] (apply krro/exe-command! cmd-id args)))

    :else nil))


(defn- make-binding
  "生成一个绑定描述 map。若 props 包含 :getter，则使用 getter；否则使用 project-binding / frame-param-binding 路径。"
  [{:keys [getter bind bindf bind-ctx]} apply-fn]
  (if getter
    (let [b {:bind-ctx bind-ctx
             :getter getter
             :apply-fn apply-fn}]
      {:binding b})
    (if bind
      {:binding {:bind-ctx bind-ctx
                 :path bind
                 :apply-fn apply-fn}}
      (if bindf
        {:frame-binding {:path bindf :apply-fn apply-fn}}
        {}))))


(defn- polyfill-action
  "如果 props 有 :setter 但没有 :on-change，则自动添加一个调用 setter 的 :on-change 回调。"
  [props]
  (if (and (:setter props) (not (:on-change props)))
    (assoc props :on-change (fn [e] ((:setter props) (:new-value e))))
    props))

;; ── 布局容器 ──────────────────────────────────────────
(defmethod create-element :block [_ props _]
  {:node (if (= (:direction props) :vertical) (VBox.) (HBox.))})

(defmethod create-element :split [_ props _]
  {:node (doto (SplitPane.)
           (.setOrientation
             (javafx.util/kw->orient (:direction props :horizontal))))})


(defmethod create-element :scroll [_ _ _] {:node (ScrollPane.)})
(defmethod create-element :tool-bar [_ _ _] {:node (ToolBar.)})
(defmethod create-element :menu-bar [_ _ _] {:node (MenuBar.)})
(defmethod create-element :menu [_ props _] {:node (Menu. (or (:content props) ""))})
(defmethod create-element :menu-item [_ props _]
  (let [item (MenuItem. (or (:content props) ""))
        tk (target-key props)]
    (event/bind-action! item (update props :on-action resolve-action) tk)
    {:node item}))

;; ── 基础控件 ──────────────────────────────────────────
(defmethod create-element :text [_ props _]
  (let [lbl (Label. (or (:content props) ""))
        tk (target-key props)]
    (event/bind-click! lbl (update props :on-click resolve-action) tk)
    (event/bind-mouse-enter-leave! lbl props tk)
    (merge
      {:node lbl}
      (make-binding props (fn [n v] (.setText ^Label n (str v)))))))

(defmethod create-element :button [_ props _]
  (let [btn (Button. (or (:content props) ""))
        tk (target-key props)]
    (event/bind-click! btn (update props :on-click resolve-action) tk)
    (event/bind-action! btn (update props :on-action resolve-action) tk)
    (event/bind-mouse-enter-leave! btn props tk)
    {:node btn}))

(defmethod create-element :input [_ props f]
  (let [props' (polyfill-action props)
        tf (TextField. (or (:content props') ""))
        tk (target-key props')]
    (event/bind-change-action! tf props' tk)   ;; 用户按回车或失去焦点时触发
    (event/bind-key! tf props' tk)
    (event/bind-focus! tf props' tk)
    (merge {:node tf}
           (make-binding props' (fn [n v] (.setText ^TextField n (str v)))))))

(defmethod create-element :text-area [_ props f]
  (let [props' (polyfill-action props)
        ta (TextArea. (or (:content props') ""))
        tk (target-key props')]
    (event/bind-change-action! ta props' tk)
    (event/bind-key! ta props' tk)
    (event/bind-focus! ta props' tk)
    (merge {:node ta}
           (make-binding props' (fn [n v] (.setText ^TextArea n (str v))))
      )))

(defmethod create-element :check-box [_ props f]
  (let [props' (polyfill-action props)
        cb (CheckBox. (or (:content props') ""))
        tk (target-key props')]
    ;; 当不存在绑定时候，使用checked?属性.
    (when-not (and (:getter props')
                   (:bind props')
                   (:bindf props'))
      (when-let [v (:checked props')] (.setSelected cb (boolean v))))
    (event/bind-change-action! cb props' tk)   ;; 用户点击复选框触发
    (event/bind-key! cb props' tk)
    (event/bind-mouse-enter-leave! cb props' tk)
    (merge {:node cb}
      (make-binding props'
                         (fn [c v]
                           (.setSelected ^CheckBox c (boolean v)))))))

(defmethod create-element :combo-box [_ props f]
  (let [props' (polyfill-action props)
        cb (ComboBox.)
        tk (target-key props')]
    (when-let [items (:items props')] (.addAll ^Collection (.getItems cb) items))
    (when-let [val (:value props')] (.setValue cb val))
    (event/bind-change-action! cb props' tk)   ;; 用户选择新项时触发
    (event/bind-key! cb props' tk)
    (merge {:node cb}
           (make-binding props' (fn [c v]
                                  (.setValue ^ComboBox c v))))))

(defmethod create-element :slider [_ props f]
  (let [props' (polyfill-action props)
        sl (Slider. (double (or (:min props') 0)) (double (or (:max props') 100)) (double (or (:value props') 50)))
        tk (target-key props')]
    (event/bind-change-action! sl props' tk)   ;; 用户拖动滑块触发
    (event/bind-key! sl props' tk)
    (merge {:node sl}
      (make-binding props' (fn [s v] (.setValue ^Slider s (double v)))))))

(defmethod create-element :progress [_ props _]
  {:node (ProgressBar. (double (or (:value props) 0)))})

(defmethod create-element :separator [_ _ _] {:node (Separator.)})
(defmethod create-element :image [_ props _] {:node (ImageView. ^String (or (:src props) ""))})


(defmethod create-element :radio-button [_ props f]
  (let [props' (polyfill-action props)
        rb (RadioButton. (or (:content props') ""))
        tk (target-key props')]
    (when-let [v (:checked props')] (.setSelected rb (boolean v)))
    (event/bind-change-action! rb props' tk)
    (event/bind-key! rb props' tk)
    (merge {:node rb}
           (make-binding props' (fn [r v] (.setSelected ^RadioButton r (boolean v))))
           )))

(defmethod create-element :link [_ props _]
  (let [hl (Hyperlink. (or (:content props) ""))
        tk (target-key props)]
    (event/bind-click! hl (update props :on-click resolve-action) tk)
    (event/bind-action! hl (update props :on-action resolve-action) tk)
    {:node hl}))
