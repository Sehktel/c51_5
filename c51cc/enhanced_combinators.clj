(ns c51cc.enhanced-combinators
  "Расширенные монадические комбинаторы для парсинга C51
   Включает контекстную обработку ошибок и специализированные комбинаторы"
  (:require [c51cc.parser :refer [ParseResult success failure current-token advance]]))

;; ============================================================================
;; КОНТЕКСТНАЯ ОБРАБОТКА ОШИБОК
;; ============================================================================

(defn with-error-context
  "Добавляет контекст к ошибкам парсинга для улучшенной диагностики
   Принимает контекстное сообщение и парсер, возвращает парсер с улучшенными ошибками"
  [context-msg parser]
  (fn [state]
    (let [result (parser state)]
      (if (:success? result)
        result
        ;; Обогащаем ошибку контекстной информацией
        (let [current-pos (:position state)
              current-tokens (take 3 (drop current-pos (:tokens state)))
              enhanced-error (str context-msg 
                                 " (позиция " current-pos 
                                 ", токены: " (mapv :value current-tokens) 
                                 ") - " (:error result))]
          (failure enhanced-error (:state result)))))))

(defn expect-with-context
  "Ожидает токен с улучшенным контекстом ошибки"
  [expected-type context-msg]
  (with-error-context 
    (str "Ожидался " expected-type " в контексте: " context-msg)
    (fn [state]
      (let [token-result (current-token state)]
        (if (:success? token-result)
          (let [token (:value token-result)]
            (if (= (:type token) expected-type)
              (let [advance-result (advance state)]
                (if (:success? advance-result)
                  (success token (:state advance-result))
                  advance-result))
              (failure (str "Ожидался " expected-type 
                           ", получен " (:type token)) state)))
          token-result)))))

;; ============================================================================
;; СПЕЦИАЛИЗИРОВАННЫЕ КОМБИНАТОРЫ ДЛЯ ВЫРАЖЕНИЙ
;; ============================================================================

(defn binary-operator-parser
  "Создает парсер для бинарных операторов с заданным приоритетом
   Использует левоассоциативность по умолчанию"
  [operand-parser operator-tokens]
  (fn [state]
    (let [left-result (operand-parser state)]
      (if (:success? left-result)
        (loop [left (:value left-result)
               current-state (:state left-result)]
          (let [token-result (current-token current-state)]
            (if (:success? token-result)
              (let [token (:value token-result)]
                (if (contains? operator-tokens (:value token))
                  (let [advance-result (advance current-state)]
                    (if (:success? advance-result)
                      (let [right-result (operand-parser (:state advance-result))]
                        (if (:success? right-result)
                          ;; Создаем узел бинарного выражения
                          (recur {:ast-type :binary-expression
                                  :operator (:value token)
                                  :left left
                                  :right (:value right-result)}
                                 (:state right-result))
                          right-result))
                      advance-result))
                  (success left current-state)))
              (success left current-state))))
        left-result))))

(defn unary-operator-parser
  "Создает парсер для унарных операторов (префиксных)"
  [operand-parser operator-tokens]
  (fn [state]
    (let [token-result (current-token state)]
      (if (:success? token-result)
        (let [token (:value token-result)]
          (if (contains? operator-tokens (:value token))
            (let [advance-result (advance state)]
              (if (:success? advance-result)
                (let [operand-result (operand-parser (:state advance-result))]
                  (if (:success? operand-result)
                    (success {:ast-type :unary-expression
                              :operator (:value token)
                              :operand (:value operand-result)}
                             (:state operand-result))
                    operand-result))
                advance-result))
            (operand-parser state)))
        (operand-parser state)))))

(defn postfix-operator-parser
  "Создает парсер для постфиксных операторов"
  [base-parser operator-tokens]
  (fn [state]
    (let [base-result (base-parser state)]
      (if (:success? base-result)
        (loop [expr (:value base-result)
               current-state (:state base-result)]
          (let [token-result (current-token current-state)]
            (if (:success? token-result)
              (let [token (:value token-result)]
                (if (contains? operator-tokens (:value token))
                  (let [advance-result (advance current-state)]
                    (if (:success? advance-result)
                      (recur {:ast-type :unary-expression
                              :operator (keyword (str "post-" (name (:value token))))
                              :operand expr}
                             (:state advance-result))
                      advance-result))
                  (success expr current-state)))
              (success expr current-state))))
        base-result))))

;; ============================================================================
;; КОМБИНАТОРЫ ДЛЯ СПИСКОВ И ПОСЛЕДОВАТЕЛЬНОСТЕЙ
;; ============================================================================

(defn separated-by
  "Парсит список элементов, разделенных указанным разделителем
   Возвращает вектор элементов (может быть пустым)"
  [element-parser separator-parser]
  (fn [state]
    (let [first-result (element-parser state)]
      (if (:success? first-result)
        ;; Есть первый элемент, парсим остальные
        (loop [elements [(:value first-result)]
               current-state (:state first-result)]
          (let [sep-result (separator-parser current-state)]
            (if (:success? sep-result)
              ;; Есть разделитель, ожидаем следующий элемент
              (let [elem-result (element-parser (:state sep-result))]
                (if (:success? elem-result)
                  (recur (conj elements (:value elem-result))
                         (:state elem-result))
                  ;; Ошибка после разделителя - это ошибка
                  elem-result))
              ;; Нет разделителя - возвращаем собранные элементы
              (success elements current-state))))
        ;; Нет первого элемента - возвращаем пустой список
        (success [] state)))))

(defn enclosed-by
  "Парсит элемент, заключенный между открывающим и закрывающим парсерами"
  [open-parser content-parser close-parser]
  (fn [state]
    (let [open-result (open-parser state)]
      (if (:success? open-result)
        (let [content-result (content-parser (:state open-result))]
          (if (:success? content-result)
            (let [close-result (close-parser (:state content-result))]
              (if (:success? close-result)
                (success (:value content-result) (:state close-result))
                close-result))
            content-result))
        open-result))))

;; ============================================================================
;; КОМБИНАТОРЫ ДЛЯ ОПЕРАТОРОВ C51
;; ============================================================================

(defn statement-block-parser
  "Специализированный парсер для блоков операторов C51
   Поддерживает правила C89/C90: объявления в начале, затем операторы"
  [declaration-parser statement-parser]
  (fn [state]
    (with-error-context "парсинг блока операторов"
      (enclosed-by
        (expect-with-context :bracket "открытие блока")
        (fn [state]
          ;; Сначала все объявления
          (let [decl-result ((c51cc.parser/many declaration-parser) state)]
            (if (:success? decl-result)
              ;; Затем все операторы
              (let [stmt-result ((c51cc.parser/many statement-parser) (:state decl-result))]
                (if (:success? stmt-result)
                  (success {:ast-type :block-statement
                            :statements (concat (:value decl-result) (:value stmt-result))}
                           (:state stmt-result))
                  stmt-result))
              decl-result)))
        (expect-with-context :bracket "закрытие блока")))))

(defn conditional-parser
  "Специализированный парсер для условных операторов"
  [condition-parser then-parser else-parser]
  (fn [state]
    (with-error-context "парсинг условного оператора"
      (fn [state]
        (let [if-result ((expect-with-context :control-keyword "ключевое слово if") state)]
          (if (:success? if-result)
            (let [cond-result (enclosed-by
                               (expect-with-context :bracket "открытие условия")
                               condition-parser
                               (expect-with-context :bracket "закрытие условия"))]
              (if (:success? cond-result)
                (let [then-result (then-parser (:state cond-result))]
                  (if (:success? then-result)
                    (let [else-result ((c51cc.parser/optional else-parser) (:state then-result))]
                      (if (:success? else-result)
                        (success {:ast-type :if-statement
                                  :condition (:value cond-result)
                                  :then-branch (:value then-result)
                                  :else-branch (:value else-result)}
                                 (:state else-result))
                        else-result))
                    then-result))
                cond-result))
            if-result))))))

;; ============================================================================
;; УТИЛИТЫ ДЛЯ ОТЛАДКИ И ПРОФИЛИРОВАНИЯ
;; ============================================================================

(defn debug-parser
  "Оборачивает парсер для отладки - выводит информацию о входе и выходе"
  [parser-name parser]
  (fn [state]
    (println (str "DEBUG: Входим в " parser-name 
                 " на позиции " (:position state)
                 ", токен: " (when-let [token (first (drop (:position state) (:tokens state)))]
                              (:value token))))
    (let [result (parser state)]
      (println (str "DEBUG: Выходим из " parser-name 
                   " с результатом: " (if (:success? result) "УСПЕХ" "ОШИБКА")))
      (when-not (:success? result)
        (println (str "DEBUG: Ошибка: " (:error result))))
      result)))

(defn profile-parser
  "Оборачивает парсер для профилирования производительности"
  [parser-name parser]
  (fn [state]
    (let [start-time (System/nanoTime)
          result (parser state)
          end-time (System/nanoTime)
          duration-ms (/ (- end-time start-time) 1000000.0)]
      (println (str "PROFILE: " parser-name " выполнился за " duration-ms " мс"))
      result))) 