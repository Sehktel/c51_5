(require '[c51cc.preprocessor-v2 :as pp])

(println "=== ОТЛАДКА ДИРЕКТИВЫ #INCLUDE ===\n")

;; Создаем простой заголовочный файл
(spit "debug.h" "#define DEBUG_VALUE 42")

(println "Содержимое debug.h:")
(println (slurp "debug.h"))

(println "\nТестируем expand-includes:")
(let [code "#include \"debug.h\"\nint x = DEBUG_VALUE;"
      initial-state (pp/create-initial-state :include-paths ["."])
      file-processor (pp/->DefaultFileProcessor)
      expand-result (pp/expand-includes code initial-state file-processor)]
  
  (println "Исходный код:" code)
  (println "Результат expand-includes:")
  (println "  content:" (:content expand-result))
  (println "  defines:" (:defines (:state expand-result)))
  (println "  errors:" (:errors expand-result)))

(println "\nТестируем полный preprocess-v2 с отладкой:")
(let [code "#include \"debug.h\"\nint x = DEBUG_VALUE;"
      initial-state (pp/create-initial-state :include-paths ["."])
      include-result (pp/expand-includes code initial-state)
      expanded-code (:content include-result)
      state-after-includes (-> initial-state
                              (assoc :defines (merge (:defines initial-state) 
                                                    (:defines (:state include-result))))
                              (assoc :errors (concat (:errors initial-state) 
                                                    (:errors (:state include-result)))))]
  
  (println "Исходный код:" code)
  (println "После expand-includes:")
  (println "  expanded-code:" expanded-code)
  (println "  initial defines:" (:defines initial-state))
  (println "  include defines:" (:defines (:state include-result)))
  (println "  merged defines:" (:defines state-after-includes))
  
  ;; Тестируем expand-macros-in-text напрямую
  (println "\nТестируем expand-macros-in-text:")
  (let [test-line "int x = DEBUG_VALUE;"
        expanded (pp/expand-macros-in-text test-line (:defines state-after-includes) state-after-includes)]
    (println "  Строка:" test-line)
    (println "  Макросы:" (:defines state-after-includes))
    (println "  Результат:" expanded)))

(println "\n=== ОТЛАДКА ЗАВЕРШЕНА ===") 