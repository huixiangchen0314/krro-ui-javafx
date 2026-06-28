(ns top.kzre.krro.ui.javafx.main
  "Krrō JavaFX 示例启动器。"
  (:require [top.kzre.krro.ui.javafx.core :as core]
            [top.kzre.krro.core.project :as proj]
            [top.kzre.krro.core.interactive :as i]
            [top.kzre.krro.core.command :as cmd]
            [top.kzre.krro.core.message :as msg]
            [top.kzre.krro.core.mode :as mode]
            [top.kzre.krro.core.keymap :as km]))

(defn -main []
  (core/launch-app
    (fn [factory renderer root-pane]
      (proj/init-project!)
      (i/set-interactor! (core/->JavaFxInteractor))

      (proj/update-project! #(assoc-in % [:test :value] "Hello World"))

      (mode/register-mode!
        (mode/make-major-mode :krro.mode/fundamental "Fundamental"
                              :parent nil
                              :keymap (km/make-keymap {})
                              :layout [:block {:direction :vertical}
                                       [:block {:direction :horizontal :style {:padding 10 :spacing 10}}
                                        [:text {:content "Krrō UI Demo"}]
                                        [:button {:content "Say Hello" :on {:click :krro.command/hello}}]]
                                       [:separator]
                                       [:input {:bind [:test :value] :placeholder "Type something..."}]
                                       [:text {:bind [:test :value] :content "Nothing yet"}]
                                       [:button {:content "Change Value" :on {:click :krro.command/change-value}}]]))

      (mode/fundamental-activate!)      ;; 触发 render-layout!

      (cmd/register-command! :krro.command/update-path
                             (fn [proj path new-val]
                               (assoc-in proj path new-val))
                             :description "通用路径更新命令，用于双向绑定")
      (cmd/register-command! :krro.command/hello
                             (fn [proj] (msg/message "Hello from Krrō!") proj)
                             :description "Say hello")
      (cmd/register-command! :krro.command/change-value
                             (fn [proj] (update-in proj [:test :value] (fn [_] (str "Updated at " (System/currentTimeMillis)))))
                             :description "Change test value"))))