;; Отладочный скрипт для анализа проблем с test_25_singleton_guard.c
;; Автор: Senior Developer

(require '[c51cc.preprocessor :as preprocessor])
(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])
(require '[clojure.pprint :as pprint])

(defn debug-test-25 []
  (println "🔍 ДЕТАЛЬНАЯ ОТЛАДКА test_25_singleton_guard.c")
  (println "=" (apply str (repeat 60 "=")))
  
  (let [file-content (slurp "c51cc/test/ccode/test_25_singleton_guard.c")]
    
    ;; Шаг 1: Показываем исходный код
    (println "\n📄 ИСХОДНЫЙ КОД:")
    (println "-" (apply str (repeat 58 "-")))
    (println file-content)
    
    ;; Шаг 2: Препроцессор
    (println "\n📝 ПОСЛЕ ПРЕПРОЦЕССОРА:")
    (println "-" (apply str (repeat 58 "-")))
    (let [clean-content (-> file-content
                           preprocessor/remove-comments
                           preprocessor/normalize-whitespace)]
      (println clean-content)
      
      ;; Шаг 3: Токены
      (println "\n🔤 ТОКЕНЫ:")
      (println "-" (apply str (repeat 58 "-")))
      (let [tokens (lexer/tokenize clean-content)]
        (println (str "Всего токенов: " (count tokens)))
        (doseq [[i token] (map-indexed vector tokens)]
          (println (format "%2d: %s" (inc i) token)))
        
        ;; Шаг 4: Попытка парсинга с детальной диагностикой
        (println "\n🌳 ПАРСИНГ:")
        (println "-" (apply str (repeat 58 "-")))
        (try
          (let [parse-state (parser/parse-state tokens)]
            (println "Начальное состояние парсера:")
            (pprint/pprint (select-keys parse-state [:position :tokens-count]))
            
            (let [result (parser/parse-program parse-state)]
              (println "\nРезультат парсинга:")
              (if (:success? result)
                (do
                  (println "✅ Парсинг успешен")
                  (println "AST:")
                  (pprint/pprint (:value result)))
                (do
                  (println "❌ Ошибка парсинга:")
                  (pprint/pprint (:error result))))))
          (catch Exception e
            (println "💥 ИСКЛЮЧЕНИЕ при парсинге:")
            (println (.getMessage e))
            (.printStackTrace e)))))))

;; Запуск отладки
(debug-test-25) 