(ns c51cc.ir.hir
  "High-level промежуточное представление (HIR) для компилятора C51
   
   HIR представляет собой промежуточный уровень между AST и MIR:
   - Сохраняет семантику высокого уровня (циклы, функции)
   - Типизированные операции
   - Упрощенная структура для оптимизаций
   - Платформо-независимые конструкции"
  (:require [clojure.spec.alpha :as s]))

;; ============================================================================
;; ТИПЫ ДАННЫХ AT89S4051
;; ============================================================================

(def at89s4051-types
  "Поддерживаемые типы данных для AT89S4051"
  #{:uint8    ; 8-битное беззнаковое (0-255)
    :int8     ; 8-битное знаковое (-128 до 127)
    :uint16   ; 16-битное беззнаковое (0-65535)
    :int16    ; 16-битное знаковое (-32768 до 32767)
    :bit      ; Битовая переменная (0 или 1)
    :pointer  ; 16-битный указатель
    :void     ; Тип void для функций
    :array    ; Массив (с дополнительной информацией о размере)
    })

(defn type-size-bytes
  "Возвращает размер типа в байтах"
  [type]
  (case type
    (:uint8 :int8 :bit) 1
    (:uint16 :int16 :pointer) 2
    :void 0
    :array nil  ; Размер зависит от элементов
    (throw (ex-info "Неизвестный тип" {:type type}))))

;; ============================================================================
;; БАЗОВЫЕ СТРУКТУРЫ HIR
;; ============================================================================

(defrecord HIR-Node 
  [op          ; Операция (ключевое слово)
   type        ; Тип данных результата
   operands    ; Вектор операндов (HIR-Node или примитивы)
   metadata])  ; Метаданные (номер строки, файл, комментарии)

;; Спецификации для валидации
(s/def ::op keyword?)
(s/def ::type at89s4051-types)
(s/def ::operands vector?)
(s/def ::metadata map?)
(s/def ::hir-node (s/keys :req-un [::op ::type ::operands ::metadata]))

;; ============================================================================
;; ОПЕРАЦИИ HIR
;; ============================================================================

(def hir-operations
  "Множество всех поддерживаемых операций HIR"
  #{;; Арифметические операции
    :add :sub :mul :div :mod
    :neg :abs
    
    ;; Битовые операции
    :and :or :xor :not
    :shl :shr
    
    ;; Операции сравнения
    :eq :ne :lt :le :gt :ge
    
    ;; Логические операции
    :logical-and :logical-or :logical-not
    
    ;; Операции присваивания
    :assign
    
    ;; Операции с памятью
    :load :store
    :array-ref :array-set
    
    ;; Управление потоком
    :if :while :for :block
    :call :return
    :break :continue
    
    ;; Приведение типов
    :cast
    
    ;; Константы и переменные
    :constant :variable
    
    ;; Специфичные для C51
    :sfr-access :sbit-access
    :interrupt-handler})

;; ============================================================================
;; КОНСТРУКТОРЫ HIR УЗЛОВ
;; ============================================================================

(defn hir-node
  "Создает HIR узел с валидацией"
  [op type operands & [metadata]]
  (let [node (->HIR-Node op type operands (or metadata {}))]
    (when-not (s/valid? ::hir-node node)
      (throw (ex-info "Некорректный HIR узел" 
                     {:node node 
                      :spec-error (s/explain-data ::hir-node node)})))
    node))

;; Арифметические операции
(defn hir-add [left right result-type & [metadata]]
  "Создает узел сложения: left + right"
  (hir-node :add result-type [left right] metadata))

(defn hir-sub [left right result-type & [metadata]]
  "Создает узел вычитания: left - right"
  (hir-node :sub result-type [left right] metadata))

(defn hir-mul [left right result-type & [metadata]]
  "Создает узел умножения: left * right"
  (hir-node :mul result-type [left right] metadata))

(defn hir-div [left right result-type & [metadata]]
  "Создает узел деления: left / right"
  (hir-node :div result-type [left right] metadata))

;; Битовые операции
(defn hir-and [left right result-type & [metadata]]
  "Создает узел битового И: left & right"
  (hir-node :and result-type [left right] metadata))

(defn hir-or [left right result-type & [metadata]]
  "Создает узел битового ИЛИ: left | right"
  (hir-node :or result-type [left right] metadata))

(defn hir-xor [left right result-type & [metadata]]
  "Создает узел битового исключающего ИЛИ: left ^ right"
  (hir-node :xor result-type [left right] metadata))

(defn hir-shl [left shift-amount result-type & [metadata]]
  "Создает узел сдвига влево: left << shift-amount"
  (hir-node :shl result-type [left shift-amount] metadata))

(defn hir-shr [left shift-amount result-type & [metadata]]
  "Создает узел сдвига вправо: left >> shift-amount"
  (hir-node :shr result-type [left shift-amount] metadata))

;; Операции сравнения
(defn hir-eq [left right & [metadata]]
  "Создает узел сравнения на равенство: left == right"
  (hir-node :eq :bit [left right] metadata))

(defn hir-lt [left right & [metadata]]
  "Создает узел сравнения меньше: left < right"
  (hir-node :lt :bit [left right] metadata))

(defn hir-le [left right & [metadata]]
  "Создает узел сравнения меньше или равно: left <= right"
  (hir-node :le :bit [left right] metadata))

;; Операции с переменными
(defn hir-variable [name type & [metadata]]
  "Создает узел переменной"
  (hir-node :variable type [name] metadata))

(defn hir-constant [value type & [metadata]]
  "Создает узел константы"
  (hir-node :constant type [value] metadata))

(defn hir-assign [dest src & [metadata]]
  "Создает узел присваивания: dest = src"
  (hir-node :assign (:type dest) [dest src] metadata))

;; Операции с памятью
(defn hir-load [variable & [metadata]]
  "Создает узел загрузки переменной"
  (hir-node :load (:type variable) [variable] metadata))

(defn hir-store [variable value & [metadata]]
  "Создает узел сохранения в переменную"
  (hir-node :store (:type variable) [variable value] metadata))

(defn hir-array-ref [array index element-type & [metadata]]
  "Создает узел доступа к элементу массива: array[index]"
  (hir-node :array-ref element-type [array index] metadata))

;; Управление потоком
(defn hir-if [condition then-branch else-branch & [metadata]]
  "Создает узел условного перехода"
  (hir-node :if :void [condition then-branch else-branch] metadata))

(defn hir-while [condition body & [metadata]]
  "Создает узел цикла while"
  (hir-node :while :void [condition body] metadata))

(defn hir-for [init condition update body & [metadata]]
  "Создает узел цикла for"
  (hir-node :for :void [init condition update body] metadata))

(defn hir-block [statements & [metadata]]
  "Создает узел блока операторов"
  (hir-node :block :void statements metadata))

(defn hir-call [function args return-type & [metadata]]
  "Создает узел вызова функции"
  (hir-node :call return-type [function args] metadata))

(defn hir-return [value & [metadata]]
  "Создает узел возврата из функции"
  (hir-node :return :void [value] metadata))

;; Приведение типов
(defn hir-cast [value target-type & [metadata]]
  "Создает узел приведения типов"
  (hir-node :cast target-type [value] metadata))

;; ============================================================================
;; УТИЛИТЫ ДЛЯ РАБОТЫ С HIR
;; ============================================================================

(defn hir-node? [x]
  "Проверяет, является ли объект HIR узлом"
  (instance? HIR-Node x))

(defn leaf-node? [node]
  "Проверяет, является ли узел листовым (без дочерних узлов)"
  (#{:variable :constant} (:op node)))

(defn binary-op? [node]
  "Проверяет, является ли узел бинарной операцией"
  (#{:add :sub :mul :div :mod :and :or :xor :shl :shr 
     :eq :ne :lt :le :gt :ge :logical-and :logical-or} (:op node)))

(defn unary-op? [node]
  "Проверяет, является ли узел унарной операцией"
  (#{:neg :abs :not :logical-not} (:op node)))

(defn control-flow-op? [node]
  "Проверяет, является ли узел операцией управления потоком"
  (#{:if :while :for :call :return :break :continue} (:op node)))

(defn get-operand [node index]
  "Безопасно получает операнд по индексу"
  (get (:operands node) index))

(defn first-operand [node]
  "Получает первый операнд"
  (get-operand node 0))

(defn second-operand [node]
  "Получает второй операнд"
  (get-operand node 1))

(defn third-operand [node]
  "Получает третий операнд"
  (get-operand node 2))

;; ============================================================================
;; ОБХОД HIR ДЕРЕВА
;; ============================================================================

(defn walk-hir
  "Обходит HIR дерево с применением функции к каждому узлу
   
   f - функция (node) -> node для трансформации узлов
   Возвращает трансформированное дерево"
  [f node]
  (if (hir-node? node)
    (let [transformed-operands (mapv #(walk-hir f %) (:operands node))
          transformed-node (assoc node :operands transformed-operands)]
      (f transformed-node))
    (f node)))

(defn collect-nodes
  "Собирает все узлы дерева, удовлетворяющие предикату"
  [pred node]
  (let [result (atom [])]
    (walk-hir (fn [n]
                (when (and (hir-node? n) (pred n))
                  (swap! result conj n))
                n)
              node)
    @result))

(defn find-variables
  "Находит все переменные в HIR дереве"
  [node]
  (collect-nodes #(= (:op %) :variable) node))

(defn find-constants
  "Находит все константы в HIR дереве"
  [node]
  (collect-nodes #(= (:op %) :constant) node))

;; ============================================================================
;; ВАЛИДАЦИЯ HIR
;; ============================================================================

(defn validate-hir-node
  "Валидирует HIR узел на корректность"
  [node]
  (cond
    (not (hir-node? node))
    {:valid? false :error "Не является HIR узлом"}
    
    (not (contains? hir-operations (:op node)))
    {:valid? false :error (str "Неизвестная операция: " (:op node))}
    
    (not (contains? at89s4051-types (:type node)))
    {:valid? false :error (str "Неподдерживаемый тип: " (:type node))}
    
    :else
    {:valid? true}))

(defn validate-hir-tree
  "Валидирует всё HIR дерево"
  [node]
  (let [errors (atom [])]
    (walk-hir (fn [n]
                (when (hir-node? n)
                  (let [validation (validate-hir-node n)]
                    (when-not (:valid? validation)
                      (swap! errors conj {:node n :error (:error validation)}))))
                n)
              node)
    (if (empty? @errors)
      {:valid? true}
      {:valid? false :errors @errors})))

;; ============================================================================
;; КРАСИВЫЙ ВЫВОД HIR
;; ============================================================================

(defn pretty-print-hir
  "Красиво выводит HIR дерево для отладки"
  [node & {:keys [indent] :or {indent 0}}]
  (let [spaces (apply str (repeat indent "  "))]
    (if (hir-node? node)
      (do
        (println (str spaces "(" (:op node) " :" (:type node)))
        (doseq [operand (:operands node)]
          (pretty-print-hir operand :indent (+ indent 1)))
        (println (str spaces ")")))
      (println (str spaces node)))))

;; ============================================================================
;; ПРИМЕРЫ ИСПОЛЬЗОВАНИЯ
;; ============================================================================

(comment
  ;; Пример создания HIR для выражения: x = a + b * 2
  (def example-hir
    (hir-assign 
      (hir-variable "x" :uint8)
      (hir-add 
        (hir-variable "a" :uint8)
        (hir-mul 
          (hir-variable "b" :uint8)
          (hir-constant 2 :uint8)
          :uint8)
        :uint8)))
  
  ;; Красивый вывод
  (pretty-print-hir example-hir)
  
  ;; Валидация
  (validate-hir-tree example-hir)
  
  ;; Поиск переменных
  (find-variables example-hir)
  )

;; Экспорт основных функций
(def ^:export hir-node hir-node)
(def ^:export validate-hir-tree validate-hir-tree)
(def ^:export pretty-print-hir pretty-print-hir)
(def ^:export walk-hir walk-hir) 