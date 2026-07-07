(ns top.kzre.krro.ui.javafx.main
  "Krrō JavaFX 示例启动器。"
  (:require [top.kzre.krro.core.command :as cmd]
            [top.kzre.krro.core.keymap :as km]
            [top.kzre.krro.core.message :as msg]
            [top.kzre.krro.core.mode :as mode]
            [top.kzre.krro.core.project :as proj]
            [top.kzre.krro.ui.javafx.core :as core]))

(defn -main []
  (core/launch-app
    (fn [factory renderer root-pane f]
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
      ;; 在当前 Frame 中激活 fundamental 模式
      (mode/activate-major-mode! :krro.mode/fundamental f)
      (cmd/register-command! :krro.command/hello
                             (fn [proj] (msg/message "Hello from Krrō!") proj)
                             :description "Say hello")
      (cmd/register-command! :krro.command/change-value
                             (fn [proj] (update-in proj [:test :value]
                                                   (fn [_] (str "Updated at " (System/currentTimeMillis)))))
                             :description "Change test value"))))