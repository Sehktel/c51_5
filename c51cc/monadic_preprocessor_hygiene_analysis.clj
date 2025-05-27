(ns c51cc.monadic-preprocessor-hygiene-analysis
  "Анализ гигиенической чистоты для монадического препроцессора
   
   КРИТИЧЕСКИЕ УРОКИ из do-parse:
   1. Автоматический gensym (symbol#) создает РАЗНЫЕ символы при каждом использовании
   2. Рекурсивные макросы создают проблемы захвата переменных
   3. Нужна валидация запрещенных имен переменных
   4. Требуется защита от конфликтов с внутренними переменными"
  (:require [clojure.string :as str]))

;; ============================================================================
;; АНАЛИЗ ПРОБЛЕМ ГИГИЕНЫ В МОНАДИЧЕСКОМ ПРЕПРОЦЕССОРЕ
;; ============================================================================

(defn analyze-hygiene-problems []
  (println "🚨 КРИТИЧЕСКИЕ ПРОБЛЕМЫ ГИГИЕНЫ В МОНАДИЧЕСКОМ ПРЕПРОЦЕССОРЕ")
  (println (str "=" (apply str (repeat 70 "="))))
  
  (println "\n📋 УРОКИ ИЗ do-parse:")
  (println "   1. ❌ symbol# создает РАЗНЫЕ символы: result# ≠ result# (в разных местах)")
  (println "   2. ❌ Рекурсивные макросы = захват переменных")
  (println "   3. ❌ Отсутствие валидации = runtime ошибки")
  (println "   4. ❌ Конфликты с пользовательскими переменными")
  
  (println "\n🎯 СПЕЦИФИЧЕСКИЕ РИСКИ для препроцессора:")
  (println "   • Переменные: state, result, error, errors, value")
  (println "   • Внутренние: preprocessor-state, file-content, macro-def")
  (println "   • Системные: filename, include-file, line-number")
  
  (println "\n⚠️  ПОСЛЕДСТВИЯ ПЛОХОЙ ГИГИЕНЫ:")
  (println "   • Непредсказуемое поведение при вложенных операциях")
  (println "   • Сложность отладки монадических цепочек")
  (println "   • Конфликты с пользовательским кодом")
  (println "   • Нарушение принципа композируемости"))

;; ============================================================================
;; ПРОБЛЕМНЫЙ КОД (как НЕ надо делать)
;; ============================================================================

(defn show-problematic-code []
  (println "\n🔴 ПРОБЛЕМНЫЙ КОД (как в первой версии do-parse):")
  (println "```clojure")
  (println "(defmacro do-result-BAD [bindings & body]")
  (println "  (if (empty? bindings)")
  (println "    `(do ~@body)")
  (println "    (let [[binding expr & rest-bindings] bindings]")
  (println "      `(bind ~expr")
  (println "             (fn [~binding]")
  (println "               (do-result-BAD [~@rest-bindings] ~@body))))))")
  (println "```")
  
  (println "\n❌ ПРОБЛЕМЫ:")
  (println "   1. Рекурсивный вызов макроса создает вложенные области")
  (println "   2. Нет защиты от захвата переменных")
  (println "   3. Отсутствует валидация входных данных")
  (println "   4. Может конфликтовать с переменными result, state, error"))

;; ============================================================================
;; ПРАВИЛЬНАЯ ГИГИЕНИЧЕСКАЯ РЕАЛИЗАЦИЯ
;; ============================================================================

(defmacro do-result
  "ГИГИЕНИЧЕСКИ ЧИСТЫЙ макрос для монадических операций Result/Either
   
   БЕЗОПАСНОСТЬ:
   - Использует явные gensym для уникальных символов
   - Валидирует входные данные
   - Защищает от захвата переменных
   - Не использует рекурсивные вызовы макроса
   
   ИСПОЛЬЗОВАНИЕ:
   (do-result
     [x (validate-input input)
      y (process-data x)
      z (save-result y)]
     (ok z))"
  [bindings & body]
  
  ;; Валидация входных данных
  (when (odd? (count bindings))
    (throw (ex-info "do-result требует четное количество аргументов в bindings" 
                   {:bindings bindings
                    :count (count bindings)})))
  
  ;; Проверка на запрещенные имена переменных
  (let [forbidden-names #{'result 'error 'errors 'value 'state 'preprocessor-state 
                          'file-content 'macro-def 'filename 'include-file 
                          'line-number 'current-state 'new-state 'final-state
                          'result# 'error# 'value# 'state#}
        binding-pairs (partition 2 bindings)]
    
    ;; Валидация имен переменных
    (doseq [[binding _] binding-pairs]
      (when (and (symbol? binding) 
                 (not= binding '_)  ; _ разрешен для игнорирования
                 (forbidden-names binding))
        (throw (ex-info "Запрещенное имя переменной в do-result - может вызвать захват переменных" 
                       {:forbidden-binding binding
                        :forbidden-names forbidden-names
                        :suggestion "Используйте другое имя переменной"}))))
    
    ;; Создаем уникальные символы ОДИН раз для всего макроса
    (let [result-sym (gensym "monadic-result")
          temp-sym (gensym "temp-value")]
      
      ;; Рекурсивная функция для построения цепочки (НЕ макрос!)
      (letfn [(build-monadic-chain [pairs final-body]
                (if (empty? pairs)
                  ;; Базовый случай - выполняем финальное тело
                  `(do ~@final-body)
                  ;; Рекурсивный случай - обрабатываем первую пару
                  (let [[binding expr] (first pairs)
                        remaining-pairs (rest pairs)]
                    `(let [~result-sym ~expr]
                       (if (ok? ~result-sym)
                         (let [~binding (:value ~result-sym)]
                           ~(build-monadic-chain remaining-pairs final-body))
                         ~result-sym)))))]  ; Возвращаем ошибку
        
        ;; Строим цепочку монадических операций
        (build-monadic-chain binding-pairs body)))))

;; ============================================================================
;; ВСПОМОГАТЕЛЬНЫЕ ГИГИЕНИЧЕСКИЕ МАКРОСЫ
;; ============================================================================

(defmacro with-error-context
  "Добавляет контекст к ошибке с гигиенической защитой"
  [context-msg expr]
  (let [result-sym (gensym "context-result")
        error-sym (gensym "context-error")]
    `(let [~result-sym ~expr]
       (if (error? ~result-sym)
         (let [~error-sym (first (:errors ~result-sym))]
           (error (str ~context-msg ": " (:message ~error-sym))
                  (merge ~error-sym {:context ~context-msg})))
         ~result-sym))))

(defmacro safe-operation
  "Выполняет операцию с автоматической обработкой исключений"
  [operation & [error-context]]
  (let [result-sym (gensym "safe-result")
        exception-sym (gensym "safe-exception")]
    `(try
       (ok ~operation)
       (catch Exception ~exception-sym
         (error (str ~(or error-context "Ошибка выполнения операции") 
                     ": " (.getMessage ~exception-sym))
                {:exception-type (type ~exception-sym)
                 :exception-message (.getMessage ~exception-sym)})))))

;; ============================================================================
;; ДЕМОНСТРАЦИЯ ПРАВИЛЬНОГО ИСПОЛЬЗОВАНИЯ
;; ============================================================================

(defn demo-hygiene-safety []
  (println "\n🟢 ДЕМОНСТРАЦИЯ ГИГИЕНИЧЕСКИ ЧИСТОГО КОДА:")
  (println "```clojure")
  (println ";; Безопасное использование do-result")
  (println "(do-result")
  (println "  [validated-input (validate-filename filename)")
  (println "   found-file (find-include-file validated-input paths)")
  (println "   file-content (read-file-safely found-file)")
  (println "   processed-content (process-includes file-content)]")
  (println "  (ok processed-content))")
  (println "```")
  
  (println "\n✅ ПРЕИМУЩЕСТВА:")
  (println "   • Каждая переменная имеет уникальное имя")
  (println "   • Нет конфликтов с внутренними переменными")
  (println "   • Автоматическое короткое замыкание при ошибках")
  (println "   • Композируемость без побочных эффектов")
  
  (println "\n🔒 ЗАЩИТА ОТ ОШИБОК:")
  (println "   • Валидация имен переменных на этапе компиляции")
  (println "   • Явные gensym вместо автоматических")
  (println "   • Отсутствие рекурсивных вызовов макроса")
  (println "   • Детальные сообщения об ошибках"))

;; ============================================================================
;; ТЕСТИРОВАНИЕ ГИГИЕНИЧЕСКОЙ ЧИСТОТЫ
;; ============================================================================

(defn test-hygiene-protection []
  (println "\n🧪 ТЕСТИРОВАНИЕ ЗАЩИТЫ ОТ ЗАХВАТА ПЕРЕМЕННЫХ:")
  
  ;; Тест 1: Запрещенные имена
  (println "\n1. Тест запрещенных имен:")
  (try
    (macroexpand '(do-result
                    [result (ok "test")]
                    (ok result)))
    (println "   ❌ Защита НЕ работает - 'result' должно быть запрещено")
    (catch Exception e
      (println "   ✅ Защита работает:" (.getMessage e))))
  
  ;; Тест 2: Разрешенные имена
  (println "\n2. Тест разрешенных имен:")
  (try
    (macroexpand '(do-result
                    [data (ok "test")
                     processed (ok (str data "!"))]
                    (ok processed)))
    (println "   ✅ Разрешенные имена работают корректно")
    (catch Exception e
      (println "   ❌ Ошибка с разрешенными именами:" (.getMessage e))))
  
  ;; Тест 3: Игнорирование результата
  (println "\n3. Тест игнорирования результата (_):")
  (try
    (macroexpand '(do-result
                    [_ (ok "ignored")
                     data (ok "important")]
                    (ok data)))
    (println "   ✅ Символ '_' работает корректно")
    (catch Exception e
      (println "   ❌ Ошибка с символом '_':" (.getMessage e)))))

;; ============================================================================
;; СРАВНЕНИЕ С ПАРСЕРОМ
;; ============================================================================

(defn compare-with-parser []
  (println "\n📊 СРАВНЕНИЕ С МАКРОСОМ do-parse ИЗ ПАРСЕРА:")
  (println (str "-" (apply str (repeat 60 "-"))))
  
  (println "\n🎯 ОБЩИЕ ПРИНЦИПЫ:")
  (println "   ✅ Явные gensym вместо автоматических")
  (println "   ✅ Валидация входных данных")
  (println "   ✅ Защита от захвата переменных")
  (println "   ✅ Отсутствие рекурсивных вызовов макроса")
  
  (println "\n🔄 РАЗЛИЧИЯ:")
  (println "   • do-parse: работает с ParseResult + State")
  (println "   • do-result: работает с Result/Either")
  (println "   • do-parse: нечетное количество аргументов")
  (println "   • do-result: четное количество аргументов")
  
  (println "\n💡 ПЕРЕИСПОЛЬЗОВАНИЕ:")
  (println "   • Можно использовать ту же инфраструктуру валидации")
  (println "   • Аналогичные паттерны защиты от захвата")
  (println "   • Схожие принципы построения цепочек"))

;; ============================================================================
;; РЕКОМЕНДАЦИИ ПО РЕАЛИЗАЦИИ
;; ============================================================================

(defn implementation-recommendations []
  (println "\n📋 РЕКОМЕНДАЦИИ ПО РЕАЛИЗАЦИИ:")
  (println (str "=" (apply str (repeat 50 "="))))
  
  (println "\n🏗️ ЭТАПЫ РЕАЛИЗАЦИИ:")
  (println "   1. Создать базовые типы Ok/Error")
  (println "   2. Реализовать функции ok?, error?, bind")
  (println "   3. Создать гигиенический макрос do-result")
  (println "   4. Добавить вспомогательные макросы")
  (println "   5. Написать комплексные тесты")
  
  (println "\n🧪 ОБЯЗАТЕЛЬНЫЕ ТЕСТЫ:")
  (println "   • Тест захвата переменных")
  (println "   • Тест валидации имен")
  (println "   • Тест композируемости")
  (println "   • Тест обработки ошибок")
  (println "   • Интеграционные тесты")
  
  (println "\n⚠️  КРИТИЧЕСКИЕ МОМЕНТЫ:")
  (println "   • НЕ использовать symbol# (автоматический gensym)")
  (println "   • НЕ делать рекурсивные вызовы макроса")
  (println "   • ВСЕГДА валидировать входные данные")
  (println "   • ОБЯЗАТЕЛЬНО тестировать гигиену"))

;; ============================================================================
;; ОСНОВНАЯ ДЕМОНСТРАЦИЯ
;; ============================================================================

(defn -main []
  (println "🔬 АНАЛИЗ ГИГИЕНИЧЕСКОЙ ЧИСТОТЫ МОНАДИЧЕСКОГО ПРЕПРОЦЕССОРА")
  (println (str "=" (apply str (repeat 80 "="))))
  
  (analyze-hygiene-problems)
  (show-problematic-code)
  (demo-hygiene-safety)
  (test-hygiene-protection)
  (compare-with-parser)
  (implementation-recommendations)
  
  (println "\n" (str "=" (apply str (repeat 80 "="))))
  (println "🎯 ЗАКЛЮЧЕНИЕ:")
  (println "   Гигиенически чистый макрос do-result КРИТИЧЕСКИ ВАЖЕН")
  (println "   для успешного монадического рефакторинга препроцессора.")
  (println "   ")
  (println "   Опыт с do-parse показывает, что без правильной гигиены")
  (println "   макрос становится источником трудноотлавливаемых багов.")
  (println (str "=" (apply str (repeat 80 "=")))))

(-main) 