(ns top.kzre.krro.ui.javafx.impl
  "JavaFX 平台实现：元素工厂与渲染器。"
  (:require
   [clojure.string :as str]
   [top.kzre.krro.core.command :as cmd]
   [top.kzre.krro.ui.core.bind :as bind]
   [top.kzre.krro.ui.core.protocol :as proto])
  (:import
   [javafx.beans.value ChangeListener]
   [javafx.event EventHandler]
   [javafx.scene Node Parent]
   [javafx.scene.control
    Button
    CheckBox
    ComboBox
    Label
    ListView
    ScrollPane
    Separator
    Slider
    Tab
    TabPane
    TextArea
    TextField]
   [javafx.scene.layout GridPane HBox VBox]))

(defn- apply-style [^Node node style-map]
  (when style-map
    (let [css-str (->> (for [[k v] style-map]
                         (str "-fx-" (name k) ": " v ";"))
                       (str/join " "))]
      (.setStyle node css-str))))

(defn- apply-common-attrs [^Node node props]
  (when-let [id (:key props)] (.setId node (name id)))
  (when-let [style (:style props)] (apply-style node style))
  (when (contains? props :visible?) (.setVisible node (boolean (:visible? props))))
  (when (contains? props :disabled?) (.setDisable node (boolean (:disabled? props))))
  node)

(defn- update-attrs [^Node node old-props new-props]
  (when (not= (:style old-props) (:style new-props))
    (if-let [style (:style new-props)]
      (apply-style node style)
      (.setStyle node "")))
  (when (not= (:visible? old-props) (:visible? new-props))
    (.setVisible node (boolean (:visible? new-props))))
  (when (not= (:disabled? old-props) (:disabled? new-props))
    (.setDisable node (boolean (:disabled? new-props))))
  (when (and (instance? Label node) (not= (:content old-props) (:content new-props)))
    (.setText ^Label node (or (:content new-props) "")))
  (when (and (instance? TextField node) (not= (:content old-props) (:content new-props)))
    (.setText ^TextField node (or (:content new-props) ""))))

(defn- bind-command [^Node node props]
  (when-let [cmd-id (get-in props [:on :click])]
    (.setOnAction node
                  (reify EventHandler
                    (handle [_ _]
                      (cmd/execute-command! cmd-id))))))

(defrecord JavaFxElementFactory []
  proto/IElementFactory
  (create-element [_ vnode]
    (let [tag (proto/node-type vnode)
          props (proto/node-props vnode)]
      (case tag
        :block
        (let [direction (:direction props :vertical)
              box (if (= direction :vertical) (VBox.) (HBox.))]
          (apply-common-attrs box props)
          box)

        :grid (let [grid (GridPane.)] (apply-common-attrs grid props) grid)

        :scroll (let [scroll (ScrollPane.)] (apply-common-attrs scroll props) scroll)

        :text
        (let [lbl (Label. (or (:content props) ""))]
          (apply-common-attrs lbl props)
          (when-let [path (:bind props)]
            (bind/register! lbl path (fn [^Label l v] (.setText l (str v)))))
          lbl)

        :button
        (let [btn (Button. (or (:content props) ""))]
          (apply-common-attrs btn props)
          (bind-command btn props)
          btn)

        :input
        (let [tf (TextField. (or (:content props) ""))]
          (apply-common-attrs tf props)
          (when-let [path (:bind props)]
            (bind/register! tf path (fn [^TextField t v] (.setText t (str v)))))
          (when-let [cmd-id (get-in props [:on :change])]
            (.addListener (.textProperty tf)
                          (proxy [ChangeListener] []
                            (changed [_ _ _ new-val]
                              (cmd/execute-command! cmd-id new-val)))))
          tf)

        :text-area
        (let [ta (TextArea. (or (:content props) ""))]
          (apply-common-attrs ta props)
          (when-let [path (:bind props)]
            (bind/register! ta path (fn [^TextArea t v] (.setText t (str v)))))
          ta)

        :check-box
        (let [cb (CheckBox. (or (:content props) ""))]
          (apply-common-attrs cb props)
          (when-let [selected? (:checked? props)] (.setSelected cb (boolean selected?)))
          (when-let [path (:bind props)]
            (bind/register! cb path (fn [^CheckBox c v] (.setSelected c (boolean v)))))
          (bind-command cb props)
          cb)

        :combo-box
        (let [cb (ComboBox.)]
          (apply-common-attrs cb props)
          (when-let [items (:items props)] (.addAll (.getItems cb) items))
          (when-let [val (:value props)] (.setValue cb val))
          (when-let [path (:bind props)]
            (bind/register! cb path (fn [^ComboBox c v] (.setValue c v))))
          (bind-command cb props)
          cb)

        :slider
        (let [sl (Slider. (double (or (:min props) 0))
                          (double (or (:max props) 100))
                          (double (or (:value props) 50)))]
          (apply-common-attrs sl props)
          (when-let [path (:bind props)]
            (bind/register! sl path (fn [^Slider s v] (.setValue s (double v)))))
          (when-let [cmd-id (get-in props [:on :change])]
            (.addListener (.valueProperty sl)
                          (proxy [ChangeListener] []
                            (changed [_ _ _ new-val]
                              (cmd/execute-command! cmd-id new-val)))))
          sl)

        :separator (Separator.)

        :tab-panel (let [tp (TabPane.)] (apply-common-attrs tp props) tp)

        :tab (let [t (Tab. (or (:title props) ""))] (apply-common-attrs t props) t)

        :image (let [img (javafx.scene.image.ImageView. (or (:src props) ""))] (apply-common-attrs img props) img)

        :list-view
        (let [lv (ListView.)]
          (apply-common-attrs lv props)
          (when-let [items (:items props)] (.addAll (.getItems lv) items))
          (when-let [path (:bind props)]
            (bind/register! lv path (fn [^ListView l v]
                                      (let [items (:items v)]
                                        (when items
                                          (.clear (.getItems l))
                                          (.addAll (.getItems l) items))))))
          (bind-command lv props)
          lv)

        (Label. (str "(unknown: " tag ")")))))

  (update-properties [_ element old-props new-props]
    (update-attrs element old-props new-props)))

(defrecord JavaFxRenderer [root-pane]
  proto/IRenderer
  (append-child [_ parent child]
    (when (instance? Parent parent)
      (.add (.getChildren ^Parent parent) child)))
  (insert-child [_ parent child index]
    (when (instance? Parent parent)
      (.add (.getChildren ^Parent parent) index child)))
  (remove-child [_ parent child]
    (when (instance? Parent parent)
      (.remove (.getChildren ^Parent parent) child)))
  (replace-child [_ parent old-child new-child]
    (when (instance? Parent parent)
      (let [children (.getChildren ^Parent parent)
            idx (.indexOf children old-child)]
        (when (>= idx 0)
          (.set children idx new-child)))))
  (move-child [_ parent child target-index]
    (when (instance? Parent parent)
      (let [children (.getChildren ^Parent parent)
            current-idx (.indexOf children child)]
        (when (and (>= current-idx 0) (not= current-idx target-index))
          (.remove children child)
          (.add children target-index child))))))