(ns test-for-final
  (:require [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]))

(defn test-for-case [description input expected-success]
  (println (str "\n=== " description " ==="))
  (println "Input:" input)
  (let [tokens (lexer/tokenize input)
        result (parser/parse tokens)]
    (println "Parse successful?:" (:success result))
    (println "Expected success?:" expected-success)
    (if (:success result)
      (let [declarations (:declarations (:ast result))]
        (println "✓ Парсинг успешен")
        (when-let [first-decl (first declarations)]
          (when (= (:ast-type first-decl) :for-statement)
            (println "  - Тип: for-statement")
            (println "  - Инициализация:" (if (:init first-decl) "есть" "нет"))
            (println "  - Условие:" (if (:condition first-decl) "есть" "нет"))
            (println "  - Обновление:" (if (:update first-decl) "есть" "нет")))))
      (if expected-success
        (println "✗ ОШИБКА: Ожидался успех, но получена ошибка:" (:error result))
        (println "✓ Ожидаемая ошибка:" (:error result))))
    (= (:success result) expected-success)))

(defn -main []
  (println "=== ФИНАЛЬНОЕ ТЕСТИРОВАНИЕ FOR-ЦИКЛОВ ДЛЯ C51 ===")
  
  (let [tests [
    ;; Корректные конструкции C51
    ["Базовый for с +=" "for (i = 0; i < 10; i++) sum += i;" true]
    ["For с -=" "for (i = 10; i > 0; i--) sum -= i;" true]
    ["For с блоком" "for (i = 0; i < 10; i++) { sum += i; count++; }" true]
    ["For без инициализации" "for (; i < 10; i++) sum += i;" true]
    ["For без условия (бесконечный цикл)" "for (i = 0; ; i++) sum += i;" true]
    ["For без обновления" "for (i = 0; i < 10; ) sum += i;" true]
    ["Пустой for (бесконечный цикл)" "for (;;) sum += i;" true]
    ["For с простым присваиванием" "for (i = 0; i < 10; i++) sum = sum + i;" true]
    
    ;; Некорректные конструкции (C99, не поддерживается в C51)
    ["For с объявлением переменной (C99)" "for (int i = 0; i < 10; i++) sum += i;" false]]
    
    results (map (fn [[description input expected]]
                   (test-for-case description input expected))
                 tests)
    passed (count (filter true? results))
    total (count tests)]
    
    (println (str "\n=== РЕЗУЛЬТАТЫ ==="))
    (println (str "Пройдено: " passed " из " total " тестов"))
    (if (= passed total)
      (println "✓ ВСЕ ТЕСТЫ ПРОЙДЕНЫ!")
      (println "✗ Есть проблемы с тестами"))))

(-main) 