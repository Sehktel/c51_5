(require '[c51cc.preprocessor :as pp])

(println "=== Отладка conditional-stack ===")

;; Модифицируем препроцессор для отладки
(in-ns 'c51cc.preprocessor)

;; Временно переопределяем process-line для отладки
(def original-process-line process-line)

(defn debug-process-line [line state]
  (let [trimmed-line (clojure.string/trim line)]
    (when (or (re-find #"#ifdef" trimmed-line)
              (re-find #"#else" trimmed-line)
              (re-find #"#endif" trimmed-line)
              (re-find #"#define" trimmed-line))
      (println "ОТЛАДКА:")
      (println "  Строка:" (pr-str line))
      (println "  Стек до:" (:conditional-stack state))
      (println "  Defines:" (keys (:defines state))))
    
    (let [[result new-state] (original-process-line line state)]
      (when (or (re-find #"#ifdef" trimmed-line)
                (re-find #"#else" trimmed-line)
                (re-find #"#endif" trimmed-line))
        (println "  Стек после:" (:conditional-stack new-state))
        (println "  Активен:" (if (empty? (:conditional-stack new-state))
                                true
                                (every? :active (:conditional-stack new-state)))))
      [result new-state])))

;; Временно заменяем функцию
(alter-var-root #'process-line (constantly debug-process-line))

(in-ns 'user)

;; Тестируем
(let [code "#define DEBUG\n#ifdef DEBUG\n#define LED P1_7\n#else\n#define LED P1_0\n#endif\nLED = 1;"
      result (pp/preprocess code)]
  (println "\nРезультат:" (pr-str result)))

;; Восстанавливаем оригинальную функцию
(in-ns 'c51cc.preprocessor)
(alter-var-root #'process-line (constantly original-process-line))
(in-ns 'user) 