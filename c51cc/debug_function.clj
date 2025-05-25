(require '[c51cc.parser :as parser])
(require '[c51cc.lexer :as lexer])

(println "🔍 ДЕТАЛЬНАЯ ДИАГНОСТИКА ФУНКЦИЙ")

(defn debug-parse [code]
  (println (str "\n=== Код: " code " ==="))
  (let [tokens (lexer/tokenize code)
        result (parser/parse tokens)]
    (println (str "Токенов: " (count tokens)))
    (println (str "Успех: " (:success result)))
    
    (if (:success result)
      (let [ast (:ast result)]
        (println (str "AST тип: " (:ast-type ast)))
        (println (str "Ключи AST: " (keys ast)))
        
        (when (:declarations ast)
          (println (str "Объявлений: " (count (:declarations ast))))
          (doseq [[i decl] (map-indexed vector (:declarations ast))]
            (println (str "  " i ": " (if decl (:ast-type decl) "nil")))
            (when decl
              (println (str "    Ключи: " (keys decl)))
              (when (= (:ast-type decl) :function-declaration)
                (println (str "    Имя: " (:name decl)))
                (println (str "    Возвращаемый тип: " (:return-type decl)))
                (println (str "    Параметры: " (count (:parameters decl))))))))
        
        ;; Проверим валидность
        (try
          (let [valid (parser/validate-ast ast)]
            (println (str "AST валиден: " valid)))
          (catch Exception e
            (println (str "Ошибка валидации: " (.getMessage e))))))
      (println (str "Ошибка: " (:error result))))))

;; Тестируем проблемные случаи
(debug-parse "void main() { }")
(debug-parse "int add(int a, int b) { return a + b; }")

;; Проверим что именно ожидают тесты
(println "\n🎯 ПРОВЕРКА ОЖИДАНИЙ ТЕСТОВ:")
(let [result (parser/parse (lexer/tokenize "void main() { }"))]
  (when (:success result)
    (let [ast (:ast result)
          func-decl (first (:declarations ast))]
      (println (str "func-decl: " func-decl))
      (when func-decl
        (println (str "func-decl тип: " (:ast-type func-decl)))
        (println (str "func-decl ключи: " (keys func-decl))))))) 