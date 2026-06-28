(ns top.kzre.krro.ui.javafx.impl
  "JavaFX 平台实现：元素工厂与节点渲染器。适配层，调用 tags 创建节点，处理通用属性。"
  (:require [clojure.string :as str]
            [top.kzre.krro.core.command :as cmd]
            [top.kzre.krro.ui.core.protocol :as proto]
            [top.kzre.krro.ui.javafx.tags :as tags])
  (:import [javafx.scene Node Parent]
           [javafx.scene.control Label TextField]))

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

(defrecord JavaFxElementFactory []
  proto/IElementFactory
  (create-element [_ vnode]
    (let [tag (proto/node-type vnode)
          props (proto/node-props vnode)
          context {:execute-command (fn [cmd-id & args] (apply cmd/execute-command! cmd-id args))}
          node (tags/render-tag tag props context)]
      (apply-common-attrs node props)
      node))
  (update-properties [_ element old-props new-props]
    (update-attrs element old-props new-props)))

(defrecord JavaFxNodeRenderer []
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