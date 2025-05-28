(ns c51cc.ast.validation.malli
  "Валидация AST через библиотеку Malli
   
   Этот модуль предоставляет:
   - Схемы Malli для всех типов узлов AST
   - Композитные схемы для сложных структур
   - Функции валидации с детальными сообщениями об ошибках
   - Генераторы тестовых данных на основе схем
   
   Преимущества Malli:
   - Декларативные схемы
   - Встроенные генераторы для тестирования
   - Производительная валидация
   - Человекочитаемые сообщения об ошибках"
  (:require [malli.core :as m]
            [malli.error :as me]
            [malli.generator :as mg]
            [malli.transform :as mt]
            [c51cc.ast.types :as types]))

;; ============================================================================
;; БАЗОВЫЕ СХЕМЫ
;; ============================================================================

(def ^:private non-empty-string
  "Схема для непустой строки"
  [:string {:min 1}])

(def ^:private ast-node-id
  "Схема для идентификатора узла"
  [:re #"^node-\d+$"])

(def ^:private source-location-schema
  "Схема для информации о позиции в исходном коде"
  [:maybe
   [:map
    [:line [:int {:min 1}]]
    [:column [:int {:min 0}]]
    [:file [:maybe :string]]]])

(def ^:private type-info-schema
  "Схема для информации о типах (заполняется семантическим анализатором)"
  [:maybe
   [:map
    [:inferred-type [:maybe :keyword]]
    [:is-lvalue [:maybe :boolean]]
    [:is-constant [:maybe :boolean]]]])

(def ^:private scope-info-schema
  "Схема для информации об области видимости"
  [:maybe
   [:map
    [:scope-level [:maybe [:int {:min 0}]]]
    [:parent-scope [:maybe :string]]]])

;; ============================================================================
;; СХЕМЫ ДЛЯ ТИПОВ УЗЛОВ
;; ============================================================================

(def ^:private ast-node-type-schema
  "Схема для типов узлов AST"
  (into [:enum] types/ast-node-types))

(def ^:private binary-operator-schema
  "Схема для бинарных операторов"
  (into [:enum] types/all-binary-operators))

(def ^:private unary-operator-schema
  "Схема для унарных операторов"
  (into [:enum] types/unary-operators))

(def ^:private literal-type-schema
  "Схема для типов литералов"
  (into [:enum] types/literal-types))

;; ============================================================================
;; БАЗОВАЯ СХЕМА УЗЛА AST
;; ============================================================================

(def ^:private base-ast-node-schema
  "Базовая схема для всех узлов AST"
  [:map
   [:ast-type ast-node-type-schema]
   [:source-location source-location-schema]
   [:type-info type-info-schema]
   [:scope-info scope-info-schema]
   [:node-id ast-node-id]])

;; ============================================================================
;; СХЕМЫ ДЛЯ КОНКРЕТНЫХ ТИПОВ УЗЛОВ
;; ============================================================================

;; Корневые узлы
(def program-schema
  "Схема для узла программы"
  [:merge base-ast-node-schema
   [:map
    [:ast-type [:= :program]]
    [:declarations [:vector [:ref ::declaration-node]]]]])

;; Объявления
(def function-declaration-schema
  "Схема для объявления функции"
  [:merge base-ast-node-schema
   [:map
    [:ast-type [:= :function-declaration]]
    [:return-type [:ref ::type-node]]
    [:name non-empty-string]
    [:parameters [:vector [:ref ::parameter-declaration-node]]]
    [:body [:ref ::statement-node]]]])

(def variable-declaration-schema
  "Схема для объявления переменной"
  [:merge base-ast-node-schema
   [:map
    [:ast-type [:= :variable-declaration]]
    [:type [:ref ::type-node]]
    [:name non-empty-string]
    [:initializer [:maybe [:ref ::expression-node]]]]])

(def parameter-declaration-schema
  "Схема для объявления параметра"
  [:merge base-ast-node-schema
   [:map
    [:ast-type [:= :parameter-declaration]]
    [:type [:ref ::type-node]]
    [:name non-empty-string]]])

;; Операторы
(def block-statement-schema
  "Схема для блока операторов"
  [:merge base-ast-node-schema
   [:map
    [:ast-type [:= :block-statement]]
    [:statements [:vector [:ref ::statement-node]]]]])

(def if-statement-schema
  "Схема для условного оператора"
  [:merge base-ast-node-schema
   [:map
    [:ast-type [:= :if-statement]]
    [:condition [:ref ::expression-node]]
    [:then-branch [:ref ::statement-node]]
    [:else-branch [:maybe [:ref ::statement-node]]]]])

(def while-statement-schema
  "Схема для цикла while"
  [:merge base-ast-node-schema
   [:map
    [:ast-type [:= :while-statement]]
    [:condition [:ref ::expression-node]]
    [:body [:ref ::statement-node]]]])

(def for-statement-schema
  "Схема для цикла for"
  [:merge base-ast-node-schema
   [:map
    [:ast-type [:= :for-statement]]
    [:init [:maybe [:or [:ref ::statement-node] [:ref ::expression-node]]]]
    [:condition [:maybe [:ref ::expression-node]]]
    [:update [:maybe [:ref ::expression-node]]]
    [:body [:ref ::statement-node]]]])

(def return-statement-schema
  "Схема для оператора return"
  [:merge base-ast-node-schema
   [:map
    [:ast-type [:= :return-statement]]
    [:expression [:maybe [:ref ::expression-node]]]]])

(def expression-statement-schema
  "Схема для оператора-выражения"
  [:merge base-ast-node-schema
   [:map
    [:ast-type [:= :expression-statement]]
    [:expression [:ref ::expression-node]]]])

(def break-statement-schema
  "Схема для оператора break"
  [:merge base-ast-node-schema
   [:map
    [:ast-type [:= :break-statement]]]])

(def continue-statement-schema
  "Схема для оператора continue"
  [:merge base-ast-node-schema
   [:map
    [:ast-type [:= :continue-statement]]]])

;; Выражения
(def binary-expression-schema
  "Схема для бинарного выражения"
  [:merge base-ast-node-schema
   [:map
    [:ast-type [:= :binary-expression]]
    [:operator binary-operator-schema]
    [:left [:ref ::expression-node]]
    [:right [:ref ::expression-node]]]])

(def unary-expression-schema
  "Схема для унарного выражения"
  [:merge base-ast-node-schema
   [:map
    [:ast-type [:= :unary-expression]]
    [:operator unary-operator-schema]
    [:operand [:ref ::expression-node]]]])

(def assignment-expression-schema
  "Схема для выражения присваивания"
  [:merge base-ast-node-schema
   [:map
    [:ast-type [:= :assignment-expression]]
    [:operator (into [:enum] types/assignment-operators)]
    [:left [:ref ::expression-node]]
    [:right [:ref ::expression-node]]]])

(def call-expression-schema
  "Схема для вызова функции"
  [:merge base-ast-node-schema
   [:map
    [:ast-type [:= :call-expression]]
    [:callee [:ref ::expression-node]]
    [:arguments [:vector [:ref ::expression-node]]]]])

(def identifier-schema
  "Схема для идентификатора"
  [:merge base-ast-node-schema
   [:map
    [:ast-type [:= :identifier]]
    [:name non-empty-string]]])

(def literal-schema
  "Схема для литерала"
  [:merge base-ast-node-schema
   [:map
    [:ast-type [:= :literal]]
    [:value :any]
    [:literal-type literal-type-schema]]])

(def array-access-schema
  "Схема для доступа к элементу массива"
  [:merge base-ast-node-schema
   [:map
    [:ast-type [:= :array-access]]
    [:array [:ref ::expression-node]]
    [:index [:ref ::expression-node]]]])

(def member-access-schema
  "Схема для доступа к члену структуры"
  [:merge base-ast-node-schema
   [:map
    [:ast-type [:= :member-access]]
    [:object [:ref ::expression-node]]
    [:member non-empty-string]
    [:is-pointer :boolean]]])

;; Специфичные для C51
(def interrupt-declaration-schema
  "Схема для объявления обработчика прерывания"
  [:merge base-ast-node-schema
   [:map
    [:ast-type [:= :interrupt-declaration]]
    [:function-name non-empty-string]
    [:interrupt-number [:int {:min 0 :max 31}]]
    [:using-clause [:maybe [:int {:min 0 :max 3}]]]]])

(def sfr-declaration-schema
  "Схема для объявления SFR"
  [:merge base-ast-node-schema
   [:map
    [:ast-type [:= :sfr-declaration]]
    [:name non-empty-string]
    [:address [:int {:min 0x80 :max 0xFF}]]]])

(def sbit-declaration-schema
  "Схема для объявления SBIT"
  [:merge base-ast-node-schema
   [:map
    [:ast-type [:= :sbit-declaration]]
    [:name non-empty-string]
    [:address [:int {:min 0x80 :max 0xFF}]]]])

(def using-declaration-schema
  "Схема для объявления using"
  [:merge base-ast-node-schema
   [:map
    [:ast-type [:= :using-declaration]]
    [:bank-number [:int {:min 0 :max 3}]]]])

;; Вспомогательная схема для типов
(def type-specifier-schema
  "Схема для спецификатора типа"
  [:merge base-ast-node-schema
   [:map
    [:ast-type [:= :type-specifier]]
    [:type (into [:enum] types/c51-data-types)]]])

;; ============================================================================
;; КОМПОЗИТНЫЕ СХЕМЫ
;; ============================================================================

(def declaration-node-schema
  "Схема для любого узла объявления"
  [:multi {:dispatch :ast-type}
   [:function-declaration function-declaration-schema]
   [:variable-declaration variable-declaration-schema]
   [:parameter-declaration parameter-declaration-schema]
   [:interrupt-declaration interrupt-declaration-schema]
   [:sfr-declaration sfr-declaration-schema]
   [:sbit-declaration sbit-declaration-schema]
   [:using-declaration using-declaration-schema]])

(def statement-node-schema
  "Схема для любого узла оператора"
  [:multi {:dispatch :ast-type}
   [:block-statement block-statement-schema]
   [:if-statement if-statement-schema]
   [:while-statement while-statement-schema]
   [:for-statement for-statement-schema]
   [:return-statement return-statement-schema]
   [:expression-statement expression-statement-schema]
   [:break-statement break-statement-schema]
   [:continue-statement continue-statement-schema]])

(def expression-node-schema
  "Схема для любого узла выражения"
  [:multi {:dispatch :ast-type}
   [:binary-expression binary-expression-schema]
   [:unary-expression unary-expression-schema]
   [:assignment-expression assignment-expression-schema]
   [:call-expression call-expression-schema]
   [:identifier identifier-schema]
   [:literal literal-schema]
   [:array-access array-access-schema]
   [:member-access member-access-schema]])

(def type-node-schema
  "Схема для узлов типов"
  [:multi {:dispatch :ast-type}
   [:type-specifier type-specifier-schema]])

;; Главная схема AST
(def ast-schema
  "Главная схема для валидации всего AST"
  [:schema {:registry {::declaration-node declaration-node-schema
                      ::statement-node statement-node-schema
                      ::expression-node expression-node-schema
                      ::type-node type-node-schema
                      ::parameter-declaration-node parameter-declaration-schema}}
   [:multi {:dispatch :ast-type}
    [:program program-schema]
    [::default [:ref ::declaration-node]]]])

;; ============================================================================
;; ФУНКЦИИ ВАЛИДАЦИИ
;; ============================================================================

(defn validate-ast-node
  "Валидирует отдельный узел AST
   
   Возвращает:
   - {:valid true} если узел валиден
   - {:valid false :errors [...]} если есть ошибки"
  [node]
  (if (m/validate ast-schema node)
    {:valid true}
    {:valid false
     :errors (me/humanize (m/explain ast-schema node))}))

(defn validate-ast-tree
  "Валидирует полное дерево AST рекурсивно
   
   Возвращает детальный отчет о всех найденных ошибках"
  [ast]
  (let [validation-result (m/explain ast-schema ast)]
    (if validation-result
      {:valid false
       :errors (me/humanize validation-result)
       :error-count (count (me/humanize validation-result))}
      {:valid true
       :message "AST прошло валидацию успешно"})))

(defn assert-valid-ast
  "Проверяет валидность AST и выбрасывает исключение при ошибке
   
   Полезно для отладки и тестирования"
  [ast]
  (let [result (validate-ast-tree ast)]
    (when-not (:valid result)
      (throw (ex-info "Невалидный AST"
                     {:type :validation-error
                      :errors (:errors result)
                      :ast ast})))
    ast))

;; ============================================================================
;; ГЕНЕРАТОРЫ ТЕСТОВЫХ ДАННЫХ
;; ============================================================================

(defn generate-ast-node
  "Генерирует случайный валидный узел AST заданного типа
   
   Полезно для property-based тестирования"
  [node-type & {:keys [size] :or {size 10}}]
  {:pre [(types/ast-node-type? node-type)]}
  (mg/generate ast-schema {:size size}))

(defn generate-random-ast
  "Генерирует случайное валидное AST дерево
   
   Аргументы:
   - max-depth: максимальная глубина дерева
   - node-count: приблизительное количество узлов"
  [& {:keys [max-depth node-count] :or {max-depth 5 node-count 20}}]
  (mg/generate ast-schema {:size node-count :max-depth max-depth}))

;; ============================================================================
;; ТРАНСФОРМАЦИИ И НОРМАЛИЗАЦИЯ
;; ============================================================================

(def ast-transformer
  "Трансформер для нормализации AST узлов"
  (mt/transformer
    {:name :ast-normalizer}
    (mt/strip-extra-keys-transformer)
    (mt/default-value-transformer)))

(defn normalize-ast
  "Нормализует AST, удаляя лишние ключи и устанавливая значения по умолчанию"
  [ast]
  (m/decode ast-schema ast ast-transformer))

;; ============================================================================
;; МЕТРИКИ И СТАТИСТИКА
;; ============================================================================

(defn validation-statistics
  "Собирает статистику валидации для анализа производительности"
  [ast-collection]
  (let [results (map validate-ast-tree ast-collection)
        valid-count (count (filter :valid results))
        invalid-count (count (filter #(not (:valid %)) results))
        total-errors (reduce + 0 (map #(get % :error-count 0) results))]
    {:total-nodes (count ast-collection)
     :valid-nodes valid-count
     :invalid-nodes invalid-count
     :total-errors total-errors
     :validation-rate (if (pos? (count ast-collection))
                       (/ valid-count (count ast-collection))
                       0)}))

;; ============================================================================
;; ЭКСПОРТ
;; ============================================================================

(def ^:export malli-validation
  "Экспорт функций валидации через Malli"
  {:validate-node validate-ast-node
   :validate-tree validate-ast-tree
   :assert-valid assert-valid-ast
   :generate-node generate-ast-node
   :generate-tree generate-random-ast
   :normalize normalize-ast
   :statistics validation-statistics
   :schema ast-schema}) 