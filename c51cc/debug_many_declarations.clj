(require '[c51cc.parser :as parser])
(require '[c51cc.lexer :as lexer])

(println "🔍 ОТЛАДКА ПАРСИНГА МНОЖЕСТВЕННЫХ ДЕКЛАРАЦИЙ")
(println "==============================================")

;; Тест: Пошаговый анализ парсинга двух деклараций
(let [code "int x = 10; void main() { }"
      tokens (lexer/tokenize code)]
  (println (str "Код: " code))
  (println (str "Токены (" (count tokens) "):"))
  (doseq [i (range (count tokens))]
    (println (str "  " i ": " (nth tokens i))))
  
  ;; Создаем начальное состояние
  (let [initial-state (parser/parse-state tokens)]
    (println (str "\nНачальное состояние: " initial-state))
    
    ;; Пробуем парсить первую декларацию
    (let [first-decl-result (parser/parse-declaration initial-state)]
      (println (str "\nПервая декларация:"))
      (println (str "  Успех: " (:success? first-decl-result)))
      (if (:success? first-decl-result)
        (do
          (println (str "  Значение: " (:value first-decl-result)))
          (println (str "  Новое состояние: " (:state first-decl-result)))
          (println (str "  Позиция после первой: " (:position (:state first-decl-result))))
          
          ;; Пробуем парсить вторую декларацию
          (let [second-decl-result (parser/parse-declaration (:state first-decl-result))]
            (println (str "\nВторая декларация:"))
            (println (str "  Успех: " (:success? second-decl-result)))
            (if (:success? second-decl-result)
              (do
                (println (str "  Значение: " (:value second-decl-result)))
                (println (str "  Позиция после второй: " (:position (:state second-decl-result)))))
              (println (str "  Ошибка: " (:error second-decl-result))))))
        (println (str "  Ошибка: " (:error first-decl-result))))))
  
  ;; Теперь тестируем функцию many
  (println "\n" (str "=" 50))
  (println "ТЕСТИРОВАНИЕ ФУНКЦИИ MANY:")
  (let [initial-state (parser/parse-state tokens)
        many-result ((parser/many parser/parse-declaration) initial-state)]
    (println (str "Результат many:"))
    (println (str "  Успех: " (:success? many-result)))
    (if (:success? many-result)
      (do
        (println (str "  Количество деклараций: " (count (:value many-result))))
        (println (str "  Декларации: " (:value many-result)))
        (println (str "  Финальная позиция: " (:position (:state many-result)))))
      (println (str "  Ошибка: " (:error many-result))))))

(println "\n✅ Отладка завершена") 