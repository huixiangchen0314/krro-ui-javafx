(ns top.kzre.krro.ui.javafx.main
  "Krrō JavaFX 示例启动器。"
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
      (proj/init-project!)
      (i/set-interactor! (core/->JavaFxInteractor))
      (core/create-stage)
      (mode/register-mode!
        (mode/make-major-mode :krro.mode/fundamental "Fundamental"
                              :parent nil
                              :keymap (km/make-keymap {})
                              :layout [:v-box
                                       [:menu-bar
                                        [:menu {:text "File"}
                                         [:menu-item {:text "Exit" :on-command :krro.command/exit}]]
                                        [:menu {:text "Edit"}
                                         [:menu-item {:text "Undo" :on-command :krro.command/undo}]]]
                                       [:tool-bar
                                        [:button {:text "Hello" :on-command :krro.command/hello}]]
                                       [:h-box {:style {:padding 10 :spacing 10}}
                                        [:label {:text "Welcome to Krrō!"}]
                                        [:button {:text "Say Hello" :on-command :krro.command/hello}]]]))
      (mode/fundamental-activate!)
      (cmd/register-command! :krro.command/hello
                             (fn [proj] (msg/message "Hello from Krrō!") proj)
                             :description "Say hello"))))