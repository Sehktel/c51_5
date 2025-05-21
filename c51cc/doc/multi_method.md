# Мультиметоды в Clojure: Функциональный подход к полиморфизму

## Введение

Мультиметоды (`defmulti` и `defmethod`) - мощный механизм динамической диспетчеризации в Clojure, который позволяет реализовать полиморфизм в функциональном стиле.

## Базовый пример

```clojure
(defmulti calculate 
  "Мультиметод для вычислений"
  (fn [operation x y] operation))

(defmethod calculate :plus [_ x y]
  (+ x y))

(defmethod calculate :minus [_ x y]
  (- x y))

(defmethod calculate :multiply [_ x y]
  (* x y))

;; Использование
(calculate :plus 2 3)     ; => 5
(calculate :minus 5 3)    ; => 2
(calculate :multiply 4 5) ; => 20
```

## Сложная диспетчеризация

```clojure
(defmulti process-data 
  "Многоуровневая диспетчеризация"
  (fn [type data]
    (cond 
      (and (number? data) (= type :square)) :numeric-square
      (and (string? data) (= type :uppercase)) :string-upper
      :else :default)))

(defmethod process-data :numeric-square [_ data]
  (* data data))

(defmethod process-data :string-upper [_ data]
  (clojure.string/upper-case data))

(defmethod process-data :default [type data]
  (throw (ex-info "Неподдерживаемая операция" 
                  {:type type :data data})))
```

## Работа с AST

### Создание узлов без использования defrecord

```clojure
(defn create-node 
  "Создание узла AST как обычной структуры"
  [type & {:as props}]
  (merge {:type type} props))

(defmulti process-node 
  "Диспетчеризация по типу узла"
  :type)

(defmethod process-node :number [node]
  (println "Число:" (:value node)))
```

### Расширенный пример с валидацией

```clojure
(def node-types 
  "Спецификация типов узлов"
  {:number     {:validator number?
                :transformer identity}
   :string     {:validator string?
                :transformer str}
   :expression {:validator map?
                :transformer #(update % :children vec)}})

(defmulti validate-node 
  "Валидация узла AST"
  :type)

(defmethod validate-node :default [node]
  (let [type-spec (get node-types (:type node))]
    (when-not type-spec
      (throw (ex-info "Неизвестный тип узла" 
                      {:node node})))
    (when-not ((:validator type-spec) node)
      (throw (ex-info "Невалидный узел" 
                      {:node node})))))
```

## Функциональный Pretty Print AST

```clojure
(defn pretty-print-ast 
  "Функциональный pretty-print AST"
  ([ast] (pretty-print-ast ast 0))
  ([ast indent]
   (let [indent-str (apply str (repeat indent "  "))]
     (case (:type ast)
       :number 
       (println indent-str "Number:" (:value ast))
       
       :binary-expression
       (do 
         (println indent-str "Binary Expression:")
         (println indent-str "  Operator:" (:operator ast))
         (println indent-str "  Left:")
         (pretty-print-ast (:left ast) (inc indent))
         (println indent-str "  Right:")
         (pretty-print-ast (:right ast) (inc indent)))
       
       :function
       (do
         (println indent-str "Function:" (:name ast))
         (println indent-str "  Parameters:" (:params ast))
         (println indent-str "  Body:")
         (pretty-print-ast (:body ast) (inc indent)))
       
       (println indent-str "Unknown node type:" (:type ast))))))
```

## Заключение

Мультиметоды в Clojure предоставляют мощный и гибкий способ реализации полиморфизма в функциональном стиле, позволяя создавать расширяемые и легко поддерживаемые системы.

### Рекомендации

1. Используйте простые структуры данных
2. Применяйте иммутабельность
3. Минимизируйте служебный код
4. Используйте композицию функций

### Сравнение подходов

| Критерий | defmulti | Pattern Matching |
|----------|----------|-----------------|
| Нативность | ✅ Встроен в Clojure | ❌ Требует библиотеки |
| Производительность | Высокая | Средняя |
| Расширяемость | Отличная | Ограничена |
| Читаемость | Средняя | Высокая |
| Гибкость | Высокая | Средняя | 