# Fixing Redundant Nested Calls in Code
_Exported on 5/27/2025 at 20:44:07 GMT+3 from Cursor (0.50.5)_

---

**User**

For the code present, we get this error:
```
Redundant nested call: and
```
How can I resolve this? If you propose a fix, please make it concise.

---

**Cursor**

Looking at the linter error and the selected code, I can see the issue is with redundant nested `and` calls. The pattern `(and (= expected-type :char) (and (= (:type token) :type-keyword) (= (:base-type token) :char)))` has an unnecessary nested `and`.

Let me search for all instances of this pattern to fix them comprehensively:

Now let me check the `expect-token-value` function for similar issues:

Теперь я вижу полную картину проблемы. Есть множество избыточных вложенных вызовов `and` в обеих функциях `expect-token` и `expect-token-value`. Исправлю это, убрав внутренние `and` и объединив все условия в один `and`:

```clojure
(defn expect-token 
  "Парсер, ожидающий конкретный тип токена"
  [expected-type]
  (fn [state]
    (let [token-result (current-token state)]
      (if (:success? token-result)
        (let [token (:value token-result)]
          (if (or (= (:type token) expected-type)
                  (and (= expected-type :number) (= (:type token) :number))
                  (and (= expected-type :identifier) (= (:type token) :identifier))
                  (and (= expected-type :identifier) (= (:type token) :main-keyword))
                  (and (= expected-type :void) (= (:type token) :type-keyword) (= (:base-type token) :void))
                  (and (= expected-type :int) (= (:type token) :type-keyword) (= (:base-type token) :int))
                  (and (= expected-type :char) (= (:type token) :type-keyword) (= (:base-type token) :char))
                  (and (= expected-type :signed) (= (:type token) :type-keyword) (= (:signedness token) :signed))
                  (and (= expected-type :unsigned) (= (:type token) :type-keyword) (= (:signedness token) :unsigned)))
            (let [advance-result (advance state)]
              (if (:success? advance-result)
                (success token (:state advance-result))
                advance-result))
            (failure (str "Ожидался токен типа " expected-type 
                         ", получен " (:type token)) state)))
        token-result))))
```

Теперь исправлю функцию `expect-token-value`:

```clojure
(defn expect-token-value 
  "Парсер, ожидающий токен с конкретным значением"
  [expected-value]
  (fn [state]
    (let [token-result (current-token state)]
      (if (:success? token-result)
        (let [token (:value token-result)]
          (if (or (= (:value token) expected-value)
                  ;; Поддержка C51 ключевых слов
                  (and (= expected-value :interrupt) (= (:type token) :c51-keyword) (= (:value token) :interrupt))
                  (and (= expected-value :using) (= (:type token) :c51-keyword) (= (:value token) :using))
                  (and (= expected-value :sfr) (= (:type token) :c51-keyword) (= (:value token) :sfr))
                  (and (= expected-value :sbit) (= (:type token) :c51-keyword) (= (:value token) :sbit))
                  ;; Существующие проверки
                  (and (= expected-value :open-round) (= (:type token) :bracket) (= (:value token) :open-round))
                  (and (= expected-value :close-round) (= (:type token) :bracket) (= (:value token) :close-round))
                  (and (= expected-value :open-curly) (= (:type token) :bracket) (= (:value token) :open-curly))
                  (and (= expected-value :close-curly) (= (:type token) :bracket) (= (:value token) :close-curly))
                  (and (= expected-value :open-square) (= (:type token) :bracket) (= (:value token) :open-square))
                  (and (= expected-value :close-square) (= (:type token) :bracket) (= (:value token) :close-square))
                  (and (= expected-value :semicolon) (= (:type token) :separator) (= (:value token) :semicolon))
                  (and (= expected-value :comma) (= (:type token) :separator) (= (:value token) :comma))
                  (and (= expected-value :plus) (= (:type token) :math-operator) (= (:value token) :plus))
                  (and (= expected-value :minus) (= (:type token) :math-operator) (= (:value token) :minus))
                  (and (= expected-value :multiply) (= (:type token) :math-operator) (= (:value token) :multiply))
                  (and (= expected-value :divide) (= (:type token) :math-operator) (= (:value token) :divide))
                  (and (= expected-value :modulo) (= (:type token) :math-operator) (= (:value token) :modulo))
                  (and (= expected-value :increment) (= (:type token) :math-operator) (= (:value token) :increment))
                  (and (= expected-value :decrement) (= (:type token) :math-operator) (= (:value token) :decrement))
                  (and (= expected-value :equal) (= (:type token) :assignment-operator) (= (:value token) :equal))
                  (and (= expected-value :plus-equal) (= (:type token) :assignment-operator) (= (:value token) :plus-equal))
                  (and (= expected-value :minus-equal) (= (:type token) :assignment-operator) (= (:value token) :minus-equal))
                  (and (= expected-value :and-equal) (= (:type token) :assignment-operator) (= (:value token) :and-equal))
                  (and (= expected-value :or-equal) (= (:type token) :assignment-operator) (= (:value token) :or-equal))
                  (and (= expected-value :xor-equal) (= (:type token) :assignment-operator) (= (:value token) :xor-equal))
                  (and (= expected-value :shift-left-equal) (= (:type token) :assignment-operator) (= (:value token) :shift-left-equal))
                  (and (= expected-value :shift-right-equal) (= (:type token) :assignment-operator) (= (:value token) :shift-right-equal))
                  (and (= expected-value :shift-left) (= (:type token) :bitwise-operator) (= (:value token) :shift-left))
                  (and (= expected-value :shift-right) (= (:type token) :bitwise-operator) (= (:value token) :shift-right))
                  (and (= expected-value :less) (= (:type token) :comparison-operator) (= (:value token) :less))
                  (and (= expected-value :greater) (= (:type token) :comparison-operator) (= (:value token) :greater))
                  (and (= expected-value :less-equal) (= (:type token) :comparison-operator) (= (:value token) :less-equal))
                  (and (= expected-value :greater-equal) (= (:type token) :comparison-operator) (= (:value token) :greater-equal))
                  (and (= expected-value :not-equal) (= (:type token) :comparison-operator) (= (:value token) :not-equal))
                  (and (= expected-value :and) (= (:type token) :logical-operator) (= (:value token) :and))
                  (and (= expected-value :or) (= (:type token) :logical-operator) (= (:value token) :or))
                  (and (= expected-value :not) (= (:type token) :logical-operator) (= (:value token) :not))
                  (and (= expected-value :if) (= (:type token) :control-keyword) (= (:value token) :if))
                  (and (= expected-value :else) (= (:type token) :control-keyword) (= (:value token) :else))
                  (and (= expected-value :while) (= (:type token) :control-keyword) (= (:value token) :while))
                  (and (= expected-value :for) (= (:type token) :control-keyword) (= (:value token) :for))
                  (and (= expected-value :return) (= (:type token) :control-keyword) (= (:value token) :return))
                  (and (= expected-value :void) (= (:type token) :type-keyword) (= (:base-type token) :void))
                  (and (= expected-value :int) (= (:type token) :type-keyword) (= (:base-type token) :int))
                  (and (= expected-value :char) (= (:type token) :type-keyword) (= (:base-type token) :char))
                  (and (= expected-value :signed) (= (:type token) :type-keyword) (= (:signedness token) :signed))
                  (and (= expected-value :unsigned) (= (:type token) :type-keyword) (= (:signedness token) :unsigned)))
            (let [advance-result (advance state)]
              (if (:success? advance-result)
                (success token (:state advance-result))
                advance-result))
            (failure (str "Ожидался токен со значением " expected-value 
                         ", получен " (:value token)) state)))
        token-result))))
```

Отлично! Я исправил все избыточные вложенные вызовы `and` в обеих функциях `expect-token` и `expect-token-value`. 

**Суть проблемы:** Код содержал паттерн `(and condition1 (and condition2 condition3))`, где внутренний `and` был избыточным.

**Решение:** Заменил все такие конструкции на `(and condition1 condition2 condition3)`, объединив все условия в один `and` без вложенности.

**Что было исправлено:**
- В `expect-token`: 5 случаев избыточных `and` для типов токенов (:void, :int, :char, :signed, :unsigned)
- В `expect-token-value`: 47 случаев избыточных `and` для различных значений токенов

Теперь код стал более читаемым и эффективным, а линтер больше не будет выдавать предупреждения о избыточных вложенных вызовах `and`.

