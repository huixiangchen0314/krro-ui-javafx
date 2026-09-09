(ns top.kzre.krro.ui.javafx.tool.compile-stylesheet
  "命令行工具：将 EDN 样式表编译为 JavaFX CSS 文件。"
  (:require
    [clojure.string :as str]
    [top.kzre.krro.ui.core.spec.stylesheet :as stylesheet]
    [top.kzre.krro.ui.javafx.stylesheet :as javafx.stylesheet]))

(defn -main
  "编译样式表命令行入口。
   用法：
     - 单个文件： compile-stylesheet input.edn [output.css]
     - 多个文件合并： compile-stylesheet input1.edn input2.edn ... output.css
     若不指定输出文件，结果输出到标准输出。"
  [& args]
  (try
    (let [arg-list (vec args)
          n (count arg-list)]
      (if (zero? n)
        (do
          (println "Usage: compile-stylesheet input.edn ... [output.css]")
          (System/exit 1))
        (let [;; 最后一个参数如果是 .css 结尾则视为输出文件
              last-arg (last arg-list)
              output-file? (and (string? last-arg) (str/ends-with? last-arg ".css"))
              output-path (when output-file? last-arg)
              input-paths (if output-file?
                            (butlast arg-list)
                            arg-list)]
          (if (empty? input-paths)
            (do
              (println "Error: no input files specified.")
              (System/exit 1))
            (let [;; 加载所有输入 EDN 并合并为一个向量
                  all-edn (mapcat (fn [path]
                                    (let [edn (stylesheet/load-stylesheet-edn path)]
                                      (if (vector? edn)
                                        edn
                                        (throw (ex-info "Invalid stylesheet: expected vector" {:path path})))))
                                  input-paths)
                  compiled (javafx.stylesheet/compile-stylesheet all-edn)]
              (if output-path
                (do
                  (spit output-path compiled)
                  (println "Compiled stylesheet written to" output-path))
                (println compiled))
              (System/exit 0))))))
      (catch Exception e
        (println "Error:" (.getMessage e))
        (System/exit 1))))