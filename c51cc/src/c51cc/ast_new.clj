(ns c51cc.ast-new
  "Главный модуль AST компилятора C51 (рефакторированная версия)
   
   Этот модуль объединяет все компоненты модульной архитектуры AST:
   - Типы узлов и константы (ast.types)
   - Конструкторы узлов (ast.constructors)
   - Валидация через Malli (ast.validation.malli)
   - Visitor pattern с мультиметодами (ast.visitor)
   
   Предоставляет единый интерфейс для работы с AST, сохраняя
   обратную совместимость с существующим кодом.
   
   Архитектурные улучшения:
   - Модульная структура для лучшей сопровождаемости
   - Расширяемая система валидации
   - Производительный visitor pattern через мультиметоды
   - Готовность к plugin системе оптимизаций"
  (:require [c51cc.ast.types :as types]
            [c51cc.ast.constructors :as constructors]
            [c51cc.ast.validation.malli :as malli-validation]
            [c51cc.ast.visitor :as visitor]
            [clojure.string :as str]
            [clojure.pprint :as pprint]))

;; ============================================================================
;; RE-EXPORT ОСНОВНЫХ КОНСТАНТ И ТИПОВ
;; ============================================================================

;; Экспортируем основные константы для обратной совместимости
(def ^:const ast-node-types types/ast-node-types)
(def ^:const binary-operators types/binary-operators)
(def ^:const unary-operators types/unary-operators)
(def ^:const all-binary-operators types/all-binary-operators)
(def ^:const operator-precedence types/operator-precedence)
(def ^:const c51-data-types types/c51-data-types)
(def ^:const literal-types types/literal-types)

;; Экспортируем предикаты типов
(def ast-node-type? types/ast-node-type?)
(def declaration-type? types/declaration-type?)
(def statement-type? types/statement-type?)
(def expression-type? types/expression-type?)
(def c51-specific-type? types/c51-specific-type?)
(def binary-operator? types/binary-operator?)
(def unary-operator? types/unary-operator?)

;; ============================================================================
;; RE-EXPORT КОНСТРУКТОРОВ
;; ============================================================================

;; Базовый конструктор
(def make-ast-node constructors/make-ast-node)

;; Конструкторы узлов (для обратной совместимости)
(def program-node constructors/program-node)
(def function-declaration-node constructors/function-declaration-node)
(def variable-declaration-node constructors/variable-declaration-node)
(def parameter-declaration-node constructors/parameter-declaration-node)
(def block-statement-node constructors/block-statement-node)
(def if-statement-node constructors/if-statement-node)
(def while-statement-node constructors/while-statement-node)
(def for-statement-node constructors/for-statement-node)
(def return-statement-node constructors/return-statement-node)
(def break-statement-node constructors/break-statement-node)
(def continue-statement-node constructors/continue-statement-node)
(def expression-statement-node constructors/expression-statement-node)
(def binary-expression-node constructors/binary-expression-node)
(def unary-expression-node constructors/unary-expression-node)
(def assignment-expression-node constructors/assignment-expression-node)
(def call-expression-node constructors/call-expression-node)
(def identifier-node constructors/identifier-node)
(def literal-node constructors/literal-node)
(def array-access-node constructors/array-access-node)
(def member-access-node constructors/member-access-node)
(def interrupt-declaration-node constructors/interrupt-declaration-node)
(def sfr-declaration-node constructors/sfr-declaration-node)
(def sbit-declaration-node constructors/sbit-declaration-node)
(def using-declaration-node constructors/using-declaration-node)

;; Фабричные функции
(def simple-function constructors/simple-function)
(def simple-variable constructors/simple-variable)

;; Утилиты для узлов
(def clone-node constructors/clone-node)
(def update-node constructors/update-node)

;; ============================================================================
;; RE-EXPORT ВАЛИДАЦИИ
;; ============================================================================

;; Основные функции валидации
(def validate-ast-node malli-validation/validate-ast-node)
(def validate-ast-tree malli-validation/validate-ast-tree)
(def assert-valid-ast malli-validation/assert-valid-ast)
(def normalize-ast malli-validation/normalize-ast)

;; Генераторы для тестирования
(def generate-ast-node malli-validation/generate-ast-node)
(def generate-random-ast malli-validation/generate-random-ast)

;; Статистика валидации
(def validation-statistics malli-validation/validation-statistics)

;; Совместимость со старой валидацией
(defn valid-ast-node?
  "Проверяет, является ли узел валидным узлом AST (совместимость)"
  [node]
  (:valid (validate-ast-node node)))

(defn validate-ast
  "Рекурсивно валидирует все узлы в AST (совместимость)
   Возвращает вектор ошибок (пустой, если AST корректен)"
  [ast]
  (let [result (validate-ast-tree ast)]
    (if (:valid result)
      []
      (or (:errors result) []))))

;; ============================================================================
;; RE-EXPORT VISITOR PATTERN
;; ============================================================================

;; Протокол и основные функции
(def ASTVisitor visitor/ASTVisitor)
(def walk-ast visitor/walk-ast)
(def walk-ast-preorder visitor/walk-ast-preorder)
(def walk-ast-postorder visitor/walk-ast-postorder)
(def walk-ast-breadth-first visitor/walk-ast-breadth-first)

;; Утилиты visitor
(def collect-nodes visitor/collect-nodes)
(def count-nodes-by-type visitor/count-nodes-by-type)
(def find-nodes visitor/find-nodes)
(def find-nodes-by-type visitor/find-nodes-by-type)
(def transform-ast visitor/transform-ast)
(def calculate-ast-depth visitor/calculate-ast-depth)

;; Специализированные функции для C51
(def collect-function-declarations visitor/collect-function-declarations)
(def collect-variable-declarations visitor/collect-variable-declarations)
(def collect-identifiers visitor/collect-identifiers)
(def collect-function-calls visitor/collect-function-calls)
(def collect-c51-specific-nodes visitor/collect-c51-specific-nodes)

;; Фабрики visitor'ов
(def make-collecting-visitor visitor/make-collecting-visitor)
(def make-counting-visitor visitor/make-counting-visitor)
(def make-filtering-visitor visitor/make-filtering-visitor)
(def make-transforming-visitor visitor/make-transforming-visitor)
(def make-debug-visitor visitor/make-debug-visitor)

;; ============================================================================
;; АНАЛИЗ AST (СОВМЕСТИМОСТЬ СО СТАРЫМ КОДОМ)
;; ============================================================================

(defn find-function-declarations
  "Находит все объявления функций в AST (совместимость)"
  [ast]
  (let [functions (collect-function-declarations ast)]
    (map (fn [func]
           {:name (:name func)
            :return-type (:return-type func)
            :parameters (count (:parameters func))
            :node func})
         functions)))

;; ============================================================================
;; КРАСИВЫЙ ВЫВОД AST
;; ============================================================================

(defn ast-to-string
  "Преобразует AST в читаемую строковую репрезентацию
   
   Улучшенная версия с поддержкой новых метаданных"
  [ast & {:keys [indent-size show-metadata show-node-ids]
          :or {indent-size 2 show-metadata false show-node-ids false}}]
  (letfn [(indent [level]
            (str/join (repeat (* level indent-size) " ")))
          
          (format-node [node level]
            (if (and (map? node) (contains? node :ast-type))
              (let [type-str (str "(" (name (:ast-type node)))
                    node-id-str (when show-node-ids
                                 (str " #" (:node-id node)))
                    attrs (cond-> (dissoc node :ast-type :node-id)
                            (not show-metadata) (dissoc :source-location 
                                                        :type-info 
                                                        :scope-info))
                    formatted-attrs (map (fn [[k v]]
                                          (str "\n" (indent (inc level))
                                               (name k) ": "
                                               (if (and (map? v) (contains? v :ast-type))
                                                 (format-node v (+ level 2))
                                                 (if (sequential? v)
                                                   (str "["
                                                        (str/join "\n"
                                                          (map #(str (indent (+ level 2))
                                                                    (format-node % (+ level 2)))
                                                               v))
                                                        (when (seq v) (str "\n" (indent (inc level))))
                                                        "]")
                                                   (pr-str v)))))
                                        attrs)]
                (str type-str node-id-str
                     (str/join formatted-attrs)
                     ")"))
              (pr-str node)))]
    
    (format-node ast 0)))

(defn pretty-print
  "Красиво выводит AST в консоль"
  [ast & options]
  (println (apply ast-to-string ast options)))

(defn print-ast-summary
  "Выводит краткую сводку по AST с улучшенной статистикой"
  [ast]
  (let [node-counts (count-nodes-by-type ast)
        depth (calculate-ast-depth ast)
        functions (find-function-declarations ast)
        identifiers (collect-identifiers ast)
        validation-result (validate-ast-tree ast)]
    
    (println "=== СВОДКА ПО AST ===")
    (println (str "Статус валидации: " (if (:valid validation-result) "✅ ВАЛИДЕН" "❌ НЕВАЛИДЕН")))
    (when-not (:valid validation-result)
      (println (str "Ошибки валидации: " (:error-count validation-result))))
    
    (println (str "Глубина дерева: " depth))
    (println (str "Общее количество узлов: " (reduce + (vals node-counts))))
    
    (println "\nКоличество узлов по типам:")
    (doseq [[type count] (sort-by first node-counts)]
      (println (str "  " (name type) ": " count)))
    
    (println (str "\nФункции (" (count functions) "):"))
    (doseq [func functions]
      (println (str "  " (:name func) " (параметров: " (:parameters func) ")")))
    
    (println (str "\nУникальные идентификаторы (" (count (set identifiers)) "):"))
    (println (str "  " (str/join ", " (sort (set identifiers)))))))

;; ============================================================================
;; ОПТИМИЗАЦИИ AST (БАЗОВЫЕ)
;; ============================================================================

(defn optimize-ast
  "Применяет базовые оптимизации к AST
   
   Улучшенная версия с использованием visitor pattern"
  [ast & {:keys [level] :or {level :O1}}]
  (case level
    :O0 ast  ; Без оптимизаций
    
    :O1 ; Базовые оптимизации
    (transform-ast ast
      (fn [node]
        (case (:ast-type node)
          ;; Свертывание константных арифметических выражений
          :binary-expression
          (let [{:keys [operator left right]} node]
            (if (and (= (:ast-type left) :literal)
                     (= (:ast-type right) :literal)
                     (= (:literal-type left) :number)
                     (= (:literal-type right) :number))
              (case operator
                :plus (literal-node (+ (:value left) (:value right)) :number)
                :minus (literal-node (- (:value left) (:value right)) :number)
                :multiply (literal-node (* (:value left) (:value right)) :number)
                :divide (if (zero? (:value right))
                         node  ; Избегаем деления на ноль
                         (literal-node (quot (:value left) (:value right)) :number))
                node)
              node))
          
          ;; Упрощение логических выражений
          :unary-expression
          (let [{:keys [operator operand]} node]
            (case operator
              :not (if (and (= (:ast-type operand) :literal)
                           (= (:literal-type operand) :boolean))
                    (literal-node (not (:value operand)) :boolean)
                    node)
              node))
          
          ;; Для остальных узлов возвращаем без изменений
          node))
      :strategy :postorder)
    
    ;; Для более высоких уровней оптимизации пока возвращаем исходный AST
    ast))

;; ============================================================================
;; УТИЛИТЫ ДЛЯ РАБОТЫ С ТИПАМИ (СОВМЕСТИМОСТЬ)
;; ============================================================================

(defn infer-expression-type
  "Выводит тип выражения на основе его структуры
   Это упрощенная версия для демонстрации концепции"
  [expr]
  (case (:ast-type expr)
    :literal
    (case (:literal-type expr)
      :number :int
      :string :char-pointer
      :char :char
      :boolean :int)
    
    :identifier
    ;; В реальной реализации здесь был бы поиск в таблице символов
    :unknown
    
    :binary-expression
    (let [left-type (infer-expression-type (:left expr))
          right-type (infer-expression-type (:right expr))]
      ;; Упрощенные правила приведения типов
      (cond
        (and (= left-type :int) (= right-type :int)) :int
        (or (= left-type :char-pointer) (= right-type :char-pointer)) :char-pointer
        :else :int))
    
    :call-expression
    ;; В реальной реализации здесь был бы поиск типа возвращаемого значения функции
    :unknown
    
    ;; Для остальных типов выражений
    :unknown))

;; ============================================================================
;; ЭКСПОРТ ОСНОВНЫХ ФУНКЦИЙ (СОВМЕСТИМОСТЬ)
;; ============================================================================

(def ^:export ast-constructors
  "Карта конструкторов узлов AST для удобного доступа (совместимость)"
  constructors/ast-constructors)

;; ============================================================================
;; МЕТРИКИ И ПРОФИЛИРОВАНИЕ
;; ============================================================================

(defn ast-performance-metrics
  "Собирает метрики производительности для AST
   
   Возвращает детальную статистику для анализа производительности"
  [ast]
  (let [start-time (System/nanoTime)
        
        ;; Базовые метрики
        node-count (count (collect-nodes ast))
        depth (calculate-ast-depth ast)
        type-counts (count-nodes-by-type ast)
        
        ;; Валидация
        validation-start (System/nanoTime)
        validation-result (validate-ast-tree ast)
        validation-time (- (System/nanoTime) validation-start)
        
        ;; Анализ сложности
        function-count (count (collect-function-declarations ast))
        expression-count (get type-counts :binary-expression 0)
        
        total-time (- (System/nanoTime) start-time)]
    
    {:performance
     {:total-time-ns total-time
      :validation-time-ns validation-time
      :nodes-per-second (if (pos? total-time) 
                         (/ (* node-count 1e9) total-time) 
                         0)}
     
     :structure
     {:node-count node-count
      :depth depth
      :function-count function-count
      :expression-count expression-count
      :complexity-score (+ (* depth 0.1) 
                          (* node-count 0.01) 
                          (* expression-count 0.05))}
     
     :validation
     {:is-valid (:valid validation-result)
      :error-count (get validation-result :error-count 0)}
     
     :distribution type-counts}))

;; ============================================================================
;; СИСТЕМА РАСШИРЕНИЙ (ЗАГОТОВКА ДЛЯ PLUGIN СИСТЕМЫ)
;; ============================================================================

(def ^:private registered-extensions (atom {}))

(defn register-ast-extension!
  "Регистрирует расширение AST (заготовка для plugin системы)
   
   ВНИМАНИЕ: Эта функция изменяет глобальное состояние!"
  [extension-name extension-config]
  {:pre [(keyword? extension-name)
         (map? extension-config)]}
  (swap! registered-extensions assoc extension-name extension-config)
  (println (str "Зарегистрировано расширение AST: " extension-name)))

(defn get-registered-extensions
  "Возвращает список зарегистрированных расширений"
  []
  @registered-extensions)

;; ============================================================================
;; ДИАГНОСТИКА И ОТЛАДКА
;; ============================================================================

(defn diagnose-ast
  "Выполняет диагностику AST и возвращает отчет о потенциальных проблемах"
  [ast]
  (let [issues (atom [])
        
        ;; Проверка валидности
        validation-result (validate-ast-tree ast)
        _ (when-not (:valid validation-result)
            (swap! issues conj {:type :validation-error
                               :severity :error
                               :message "AST не прошло валидацию"
                               :details (:errors validation-result)}))
        
        ;; Проверка глубины
        depth (calculate-ast-depth ast)
        _ (when (> depth 20)
            (swap! issues conj {:type :excessive-depth
                               :severity :warning
                               :message (str "Слишком глубокое AST: " depth " уровней")
                               :recommendation "Рассмотрите рефакторинг кода"}))
        
        ;; Проверка количества узлов
        node-count (count (collect-nodes ast))
        _ (when (> node-count 1000)
            (swap! issues conj {:type :large-ast
                               :severity :info
                               :message (str "Большое AST: " node-count " узлов")
                               :recommendation "Рассмотрите разбиение на модули"}))
        
        ;; Проверка типов узлов
        type-counts (count-nodes-by-type ast)
        _ (when (> (get type-counts :binary-expression 0) 100)
            (swap! issues conj {:type :complex-expressions
                               :severity :warning
                               :message "Много сложных выражений"
                               :recommendation "Рассмотрите упрощение логики"}))]
    
    {:status (if (empty? @issues) :healthy :issues-found)
     :issues @issues
     :metrics (ast-performance-metrics ast)
     :recommendations 
     (when (seq @issues)
       ["Запустите валидацию для исправления ошибок"
        "Рассмотрите применение оптимизаций"
        "Проверьте архитектуру кода на предмет упрощения"])}))

;; ============================================================================
;; ФИНАЛЬНЫЙ ЭКСПОРТ
;; ============================================================================

(def ^:export ast-api
  "Полный API модуля AST для внешнего использования"
  {:types types/type-definitions
   :constructors constructors/ast-constructors
   :validation malli-validation/malli-validation
   :visitor visitor/visitor-functions
   :analysis {:collect-nodes collect-nodes
             :count-nodes count-nodes-by-type
             :find-nodes find-nodes
             :calculate-depth calculate-ast-depth
             :performance-metrics ast-performance-metrics
             :diagnose diagnose-ast}
   :optimization {:optimize optimize-ast}
   :utilities {:pretty-print pretty-print
              :ast-to-string ast-to-string
              :print-summary print-ast-summary}}) 