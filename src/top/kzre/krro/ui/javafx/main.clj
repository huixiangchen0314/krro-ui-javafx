(ns top.kzre.krro.ui.javafx.main
  "Krrō JavaFX 示例启动器，演示基础控件、菜单、命令与数据绑定。"
  (:require [top.kzre.krro.core.command :as cmd]
            [top.kzre.krro.core.interactive :as i]
            [top.kzre.krro.core.keymap :as km]
            [top.kzre.krro.core.message :as msg]
            [top.kzre.krro.core.mode :as mode]
            [top.kzre.krro.core.project :as proj]
            [top.kzre.krro.ui.javafx.core :as core]))

(defn -main []
  (core/launch-app
    (fn []
      ;; 初始化项目
      (proj/init-project!)
      ;; 安装交互器
      (i/set-interactor! (core/->JavaFxInteractor))
      ;; 创建主舞台
      (core/create-stage)
      ;; 注册 Fundamental 模式，包含菜单栏、工具栏和内容区
      (mode/register-mode!
        (mode/make-major-mode :krro.mode/fundamental "Fundamental"
                              :parent nil
                              :keymap (km/make-keymap {})
                              :layout
                              [:v-box
                               ;; 菜单栏
                               [:menu-bar
                                [:menu {:text "File"}
                                 [:menu-item {:text "Exit" :on-command :krro.command/exit}]]
                                [:menu {:text "Edit"}
                                 [:menu-item {:text "Undo" :on-command :krro.command/undo}]]]
                               ;; 工具栏
                               [:tool-bar
                                [:button {:text "Hello" :on-command :krro.command/hello}]]
                               ;; 内容区（水平盒子）
                               [:h-box {:style {:padding 10 :spacing 10}}
                                [:label {:text "Welcome to Krrō!"}]
                                [:button {:text "Say Hello" :on-command :krro.command/hello}]
                                [:separator]
                                ;; 带数据绑定的文本框和标签
                                [:text-field {:bind [:user :name] :placeholder "Your name"}]
                                [:label {:bind [:user :name] :text "No name"}]
                                ;; 按钮修改绑定数据
                                [:button {:text "Change Name" :on-command :krro.command/change-name}]]]))
      ;; 激活 Fundamental 模式
      (mode/fundamental-activate!)
      ;; 注册命令
      (cmd/register-command! :krro.command/hello
                             (fn [proj]
                               (msg/message "Hello from Krrō!")
                               proj)
                             :description "Say hello")
      (cmd/register-command! :krro.command/change-name
                             (fn [proj]
                               (assoc-in proj [:user :name] (str "User-" (rand-int 1000))))
                             :description "Change the user name to a random value")
      ;; 初始化项目中的用户数据
      (proj/update-project! #(assoc-in % [:user :name] "Alice")))))