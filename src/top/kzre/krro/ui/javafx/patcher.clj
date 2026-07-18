(ns top.kzre.krro.ui.javafx.patcher
  (:require [top.kzre.krro.ui.core.protocol :as proto])
  (:import (java.util List)
           (javafx.scene Parent)))

;; ═══════════════════════════════════════════════════════
;; 平台多方法（根据容器类名分派）
;; ═══════════════════════════════════════════════════════

(defmulti append-node  (fn [parent _child]          (when parent (.getName (class parent)))))
(defmulti insert-node  (fn [parent _child _index]    (when parent (.getName (class parent)))))
(defmulti remove-node  (fn [parent _child]          (when parent (.getName (class parent)))))
(defmulti replace-node (fn [parent _old _new]        (when parent (.getName (class parent)))))
(defmulti move-node    (fn [parent _child _target]   (when parent (.getName (class parent)))))

;; ── 安全辅助 ─────────────────────────────────
(defn- get-children ^List [^Parent parent]
  (.getChildren parent))

;; ── 默认实现（任何 Parent 容器，安全处理非 Parent） ─────
(defmethod append-node :default [parent child]
  (if (instance? Parent parent)
    (.add (get-children parent) child)
    (throw (UnsupportedOperationException. (str "Cannot append to " (class parent))))))
(defmethod insert-node :default [parent child index]
  (if (instance? Parent parent)
    (.add (get-children parent) index child)
    (throw (UnsupportedOperationException. (str "Cannot insert into " (class parent))))))
(defmethod remove-node :default [parent child]
  (if (instance? Parent parent)
    (.remove (get-children parent) child)
    (throw (UnsupportedOperationException. (str "Cannot remove from " (class parent))))))
(defmethod replace-node :default [parent old new]
  (if (instance? Parent parent)
    (let [children (get-children parent)
          idx (.indexOf children old)]
      (when (>= idx 0) (.set children idx new)))
    (throw (UnsupportedOperationException. (str "Cannot replace in " (class parent))))))
(defmethod move-node :default [parent child target-index]
  (if (instance? Parent parent)
    (let [children (get-children parent)
          idx (.indexOf children child)]
      (when (and (>= idx 0) (not= idx target-index))
        (.remove children child)
        (.add children (min target-index (.size children)) child)))
    (throw (UnsupportedOperationException. (str "Cannot move in " (class parent))))))

;; ── MenuBar ────────────────────────────────────
(defmethod append-node "javafx.scene.control.MenuBar" [parent child]
  (.add (.getMenus parent) child))
(defmethod insert-node "javafx.scene.control.MenuBar" [parent child index]
  (.add (.getMenus parent) index child))
(defmethod remove-node "javafx.scene.control.MenuBar" [parent child]
  (.remove (.getMenus parent) child))
(defmethod replace-node "javafx.scene.control.MenuBar" [parent old new]
  (let [items (.getMenus parent)
        idx (.indexOf items old)]
    (when (>= idx 0) (.set items idx new))))
(defmethod move-node "javafx.scene.control.MenuBar" [parent child target-index]
  (let [items (.getMenus parent)
        idx (.indexOf items child)]
    (when (and (>= idx 0) (not= idx target-index))
      (.remove items child)
      (.add items (min target-index (.size items)) child))))

;; ── Menu ───────────────────────────────────────
(defmethod append-node "javafx.scene.control.Menu" [parent child]
  (.add (.getItems parent) child))
(defmethod insert-node "javafx.scene.control.Menu" [parent child index]
  (.add (.getItems parent) index child))
(defmethod remove-node "javafx.scene.control.Menu" [parent child]
  (.remove (.getItems parent) child))
(defmethod replace-node "javafx.scene.control.Menu" [parent old new]
  (let [items (.getItems parent)
        idx (.indexOf items old)]
    (when (>= idx 0) (.set items idx new))))
(defmethod move-node "javafx.scene.control.Menu" [parent child target-index]
  (let [items (.getItems parent)
        idx (.indexOf items child)]
    (when (and (>= idx 0) (not= idx target-index))
      (.remove items child)
      (.add items (min target-index (.size items)) child))))

;; ── MenuItem（禁止子节点）──────────────────────
(defmethod append-node "javafx.scene.control.MenuItem" [parent _]
  (throw (UnsupportedOperationException. "MenuItem cannot have children")))

;; ── ToolBar ────────────────────────────────────
(defmethod append-node "javafx.scene.control.ToolBar" [parent child]
  (.add (.getItems parent) child))
(defmethod insert-node "javafx.scene.control.ToolBar" [parent child index]
  (.add (.getItems parent) index child))
(defmethod remove-node "javafx.scene.control.ToolBar" [parent child]
  (.remove (.getItems parent) child))
(defmethod replace-node "javafx.scene.control.ToolBar" [parent old new]
  (let [items (.getItems parent)
        idx (.indexOf items old)]
    (when (>= idx 0) (.set items idx new))))
(defmethod move-node "javafx.scene.control.ToolBar" [parent child target-index]
  (let [items (.getItems parent)
        idx (.indexOf items child)]
    (when (and (>= idx 0) (not= idx target-index))
      (.remove items child)
      (.add items (min target-index (.size items)) child))))

;; ── TabPane ────────────────────────────────────
(defmethod append-node "javafx.scene.control.TabPane" [parent child]
  (.add (.getTabs parent) child))
(defmethod insert-node "javafx.scene.control.TabPane" [parent child index]
  (.add (.getTabs parent) index child))
(defmethod remove-node "javafx.scene.control.TabPane" [parent child]
  (.remove (.getTabs parent) child))
(defmethod replace-node "javafx.scene.control.TabPane" [parent old new]
  (let [tabs (.getTabs parent)
        idx (.indexOf tabs old)]
    (when (>= idx 0) (.set tabs idx new))))
(defmethod move-node "javafx.scene.control.TabPane" [parent child target-index]
  (let [tabs (.getTabs parent)
        idx (.indexOf tabs child)]
    (when (and (>= idx 0) (not= idx target-index))
      (.remove tabs child)
      (.add tabs (min target-index (.size tabs)) child))))

;; ── SplitPane ──────────────────────────────────
(defmethod append-node "javafx.scene.control.SplitPane" [parent child]
  (.add (.getItems parent) child))
(defmethod insert-node "javafx.scene.control.SplitPane" [parent child index]
  (.add (.getItems parent) index child))
(defmethod remove-node "javafx.scene.control.SplitPane" [parent child]
  (.remove (.getItems parent) child))
(defmethod replace-node "javafx.scene.control.SplitPane" [parent old new]
  (let [items (.getItems parent)
        idx (.indexOf items old)]
    (when (>= idx 0) (.set items idx new))))
(defmethod move-node "javafx.scene.control.SplitPane" [parent child target-index]
  (let [items (.getItems parent)
        idx (.indexOf items child)]
    (when (and (>= idx 0) (not= idx target-index))
      (.remove items child)
      (.add items (min target-index (.size items)) child))))

;; ═══════════════════════════════════════════════════════
;; 协议实现（仅包含 INodePatcher 定义的五个方法）
;; ═══════════════════════════════════════════════════════
(defrecord JavaFxNodePatcher []
  proto/INodePatcher
  (append-child [_ parent child]       (append-node parent child))
  (insert-child [_ parent child index] (insert-node parent child index))
  (remove-child [_ parent child]       (remove-node parent child))
  (replace-child [_ parent old new]    (replace-node parent old new))
  (move-child [_ parent child idx]     (move-node parent child idx)))