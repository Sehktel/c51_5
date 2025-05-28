(defproject c51cc "0.1.0"
  :description "Clojure C Compiler"
  :url "https://github.com/Sehktel/c51cc"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 
                 ;; Валидация схем (для AST рефакторинга)
                 [metosin/malli "0.13.0"]           ; Валидация через Malli
                 [prismatic/schema "1.4.1"]         ; Альтернативная валидация
                 
                 ;; Pattern matching (для visitor pattern)
                 [org.clojure/core.match "1.0.1"]   ; Pattern matching
                 
                 ;; Профилирование и тестирование
                 [criterium "0.4.6"]                ; Микробенчмарки
                 [org.clojure/test.check "1.1.1"]]  ; Property-based тестирование
  :main ^:skip-aot c51cc.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}
             :dev {:dependencies [[org.clojure/tools.namespace "1.4.4"]
                                  [org.clojure/tools.trace "0.7.11"]]}})
