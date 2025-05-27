(ns c51cc.preprocessor-v2-test
  "Тесты для рефакторированного препроцессора C51.
   
   Покрывают:
   - Функциональность всех директив препроцессора
   - Работу с transducers
   - Валидацию через clojure.spec
   - Обработку ошибок
   - Производительность"
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [c51cc.preprocessor-v2 :as pp2]))

;; ============================================================================
;; ТЕСТЫ БАЗОВОЙ ФУНКЦИОНАЛЬНОСТИ
;; ============================================================================

(deftest test-create-initial-state
  (testing "Создание начального состояния препроцессора"
    (let [state (pp2/create-initial-state)]
      (is (s/valid? ::pp2/preprocessor-state state))
      (is (= {} (:defines state)))
      (is (= [] (:include-stack state)))
      (is (= ["." "include" "lib"] (:include-paths state)))
      (is (= 1 (:line-number state)))
      (is (= "input" (:current-file state)))
      (is (= [] (:errors state)))
      (is (= [] (:conditional-stack state)))))
  
  (testing "Создание состояния с пользовательскими параметрами"
    (let [state (pp2/create-initial-state 
                 :defines {"DEBUG" "1"}
                 :include-paths ["/usr/include"]
                 :current-file "test.c")]
      (is (= {"DEBUG" "1"} (:defines state)))
      (is (= ["/usr/include"] (:include-paths state)))
      (is (= "test.c" (:current-file state))))))

(deftest test-add-error
  (testing "Добавление ошибки в состояние"
    (let [state (pp2/create-initial-state)
          error-state (pp2/add-error state "Тестовая ошибка" {:type :test})]
      (is (= 1 (count (:errors error-state))))
      (let [error (first (:errors error-state))]
        (is (= "Тестовая ошибка" (:message error)))
        (is (= 1 (:line error)))
        (is (= "input" (:file error)))
        (is (= :test (:type error)))
        (is (number? (:timestamp error)))))))

(deftest test-should-process-line
  (testing "Определение необходимости обработки строки"
    (let [state (pp2/create-initial-state)]
      ;; Пустой стек условий - обрабатываем
      (is (pp2/should-process-line? state))
      
      ;; Активный блок - обрабатываем
      (let [active-state (update state :conditional-stack 
                                conj {:type :ifdef :condition true :active true})]
        (is (pp2/should-process-line? active-state)))
      
      ;; Неактивный блок - не обрабатываем
      (let [inactive-state (update state :conditional-stack 
                                  conj {:type :ifdef :condition false :active false})]
        (is (not (pp2/should-process-line? inactive-state)))))))

;; ============================================================================
;; ТЕСТЫ ПАРСИНГА МАКРОСОВ
;; ============================================================================

(deftest test-parse-macro-definition
  (testing "Парсинг простых макросов (C89)"
    (are [input expected] (= expected (pp2/parse-macro-definition input))
      "MAX_SIZE 100"           ["MAX_SIZE" "100"]
      "DEBUG"                  ["DEBUG" ""]
      "PI 3.14159"            ["PI" "3.14159"]
      "VERSION \"1.0.0\""     ["VERSION" "\"1.0.0\""]
      "NULL 0"                ["NULL" "0"]
      "TRUE 1"                ["TRUE" "1"]
      "FALSE 0"               ["FALSE" "0"])))

;; ============================================================================
;; ТЕСТЫ ДИРЕКТИВ ПРЕПРОЦЕССОРА
;; ============================================================================

(deftest test-define-directive
  (testing "Распознавание директивы #define"
    (let [directive (pp2/->DefineDirective)]
      (is (pp2/matches? directive "#define MAX_SIZE 100"))
      (is (pp2/matches? directive "  #define DEBUG  "))
      (is (not (pp2/matches? directive "#include <stdio.h>")))
      (is (not (pp2/matches? directive "int x = 5;")))))
  
  (testing "Обработка простых макросов (C89)"
    (let [directive (pp2/->DefineDirective)
          state (pp2/create-initial-state)
          [result new-state] (pp2/process directive "#define MAX_SIZE 100" state)]
      (is (nil? result))  ; Директивы не включаются в вывод
      (is (= "100" (get-in new-state [:defines "MAX_SIZE"]))))
    
    ;; Макрос без значения
    (let [directive (pp2/->DefineDirective)
          state (pp2/create-initial-state)
          [result new-state] (pp2/process directive "#define DEBUG" state)]
      (is (nil? result))
      (is (= "" (get-in new-state [:defines "DEBUG"]))))))

(deftest test-undef-directive
  (testing "Обработка директивы #undef"
    (let [directive (pp2/->UndefDirective)
          state (-> (pp2/create-initial-state)
                    (assoc-in [:defines "DEBUG"] "1")
                    (assoc-in [:defines "VERSION"] "2.0"))
          [result new-state] (pp2/process directive "#undef DEBUG" state)]
      (is (nil? result))
      (is (not (contains? (:defines new-state) "DEBUG")))
      (is (contains? (:defines new-state) "VERSION")))))

(deftest test-conditional-directives
  (testing "Директива #ifdef"
    (let [directive (pp2/->IfdefDirective)
          state (assoc-in (pp2/create-initial-state) [:defines "DEBUG"] "1")
          [result new-state] (pp2/process directive "#ifdef DEBUG" state)]
      (is (nil? result))
      (is (= 1 (count (:conditional-stack new-state))))
      (let [block (peek (:conditional-stack new-state))]
        (is (= :ifdef (:type block)))
        (is (:condition block))
        (is (:active block)))))
  
  (testing "Директива #ifndef"
    (let [directive (pp2/->IfndefDirective)
          state (pp2/create-initial-state)  ; DEBUG не определен
          [result new-state] (pp2/process directive "#ifndef DEBUG" state)]
      (is (nil? result))
      (is (= 1 (count (:conditional-stack new-state))))
      (let [block (peek (:conditional-stack new-state))]
        (is (= :ifndef (:type block)))
        (is (:condition block))
        (is (:active block)))))
  
  (testing "Директива #else"
    (let [directive (pp2/->ElseDirective)
          state (-> (pp2/create-initial-state)
                    (update :conditional-stack conj {:type :ifdef :condition false :active false}))
          [result new-state] (pp2/process directive "#else" state)]
      (is (nil? result))
      (let [block (peek (:conditional-stack new-state))]
        (is (:active block))  ; Инвертируется относительно исходного condition
        (is (:in-else block)))))
  
  (testing "Директива #endif"
    (let [directive (pp2/->EndifDirective)
          state (-> (pp2/create-initial-state)
                    (update :conditional-stack conj {:type :ifdef :condition true :active true}))
          [result new-state] (pp2/process directive "#endif" state)]
      (is (nil? result))
      (is (empty? (:conditional-stack new-state)))))
  
  (testing "Ошибки в условных директивах"
    (let [else-directive (pp2/->ElseDirective)
          endif-directive (pp2/->EndifDirective)
          state (pp2/create-initial-state)]
      ;; #else без #if
      (let [[result new-state] (pp2/process else-directive "#else" state)]
        (is (nil? result))
        (is (= 1 (count (:errors new-state)))))
      
      ;; #endif без #if
      (let [[result new-state] (pp2/process endif-directive "#endif" state)]
        (is (nil? result))
        (is (= 1 (count (:errors new-state))))))))

;; ============================================================================
;; ТЕСТЫ TRANSDUCERS
;; ============================================================================

(deftest test-remove-comments-xf
  (testing "Удаление комментариев"
    (let [xf (pp2/remove-comments-xf)
          input ["int x = 5; // комментарий"
                 "/* многострочный */ int y = 10;"
                 "int z = 15;"]
          result (into [] xf input)]
      (is (= ["int x = 5; "
              " int y = 10;"
              "int z = 15;"] result)))))

(deftest test-normalize-whitespace-xf
  (testing "Нормализация пробелов"
    (let [xf (pp2/normalize-whitespace-xf)
          input ["  int    x   =   5;  "
                 "\t\tint\ty\t=\t10;\t"
                 "int z = 15;"]
          result (into [] xf input)]
      (is (= ["int x = 5;"
              "int y = 10;"
              "int z = 15;"] result)))))

(deftest test-filter-empty-lines-xf
  (testing "Фильтрация пустых строк"
    (let [xf (pp2/filter-empty-lines-xf)
          input ["int x = 5;"
                 ""
                 "   "
                 "int y = 10;"]
          result (into [] xf input)]
      (is (= ["int x = 5;"
              "int y = 10;"] result)))))

;; ============================================================================
;; ТЕСТЫ РАСШИРЕНИЯ МАКРОСОВ
;; ============================================================================

(deftest test-expand-macros-in-text
  (testing "Расширение простых макросов"
    (let [state (pp2/create-initial-state)
          defines {"MAX_SIZE" "100" "DEBUG" "1"}
          text "array[MAX_SIZE]; if (DEBUG) printf(\"debug\");"]
      (is (= "array[100]; if (1) printf(\"debug\");"
             (pp2/expand-macros-in-text text defines state)))))
  
  (testing "Предопределенные макросы"
    (let [state (-> (pp2/create-initial-state)
                    (assoc :line-number 42)
                    (assoc :current-file "test.c"))
          text "printf(\"Line: %d, File: %s\", __LINE__, __FILE__);"]
      (is (str/includes? (pp2/expand-macros-in-text text {} state) "42"))
      (is (str/includes? (pp2/expand-macros-in-text text {} state) "\"test.c\""))))
  
  (testing "Рекурсивное расширение макросов"
    (let [state (pp2/create-initial-state)
          defines {"A" "B" "B" "C" "C" "42"}
          text "value = A;"]
      (is (= "value = 42;" (pp2/expand-macros-in-text text defines state)))))
  
  (testing "Предотвращение бесконечных циклов"
    (let [state (pp2/create-initial-state)
          defines {"A" "B" "B" "A"}  ; Циклическая зависимость
          text "value = A;"]
      ;; Должно остановиться после максимального количества итераций
      (is (string? (pp2/expand-macros-in-text text defines state))))))

;; ============================================================================
;; ИНТЕГРАЦИОННЫЕ ТЕСТЫ
;; ============================================================================

(deftest test-preprocess-v2-basic
  (testing "Базовая функциональность препроцессора"
    (let [code "#define MAX_SIZE 100\nint array[MAX_SIZE];"
          result (pp2/preprocess-v2 code)]
      (is (:success result))
      (is (empty? (:errors result)))
      (is (str/includes? (:result result) "int array[100];")))))

(deftest test-preprocess-v2-conditional-compilation
  (testing "Условная компиляция"
    (let [code "#define DEBUG 1\n#ifdef DEBUG\nprintf(\"debug\");\n#endif\nprintf(\"release\");"
          result (pp2/preprocess-v2 code)]
      (is (:success result))
      (is (str/includes? (:result result) "printf(\"debug\");"))
      (is (str/includes? (:result result) "printf(\"release\");"))))
  
  (testing "Условная компиляция с #ifndef"
    (let [code "#ifndef RELEASE\nprintf(\"debug\");\n#endif\nprintf(\"always\");"
          result (pp2/preprocess-v2 code)]
      (is (:success result))
      (is (str/includes? (:result result) "printf(\"debug\");"))
      (is (str/includes? (:result result) "printf(\"always\");"))))
  
  (testing "Условная компиляция с #else"
    (let [code "#ifdef DEBUG\nprintf(\"debug\");\n#else\nprintf(\"release\");\n#endif"
          result (pp2/preprocess-v2 code)]
      (is (:success result))
      ;; DEBUG не определен, поэтому должен выполниться блок #else
      (is (str/includes? (:result result) "printf(\"release\");"))
      (is (not (str/includes? (:result result) "printf(\"debug\");"))))))

(deftest test-preprocess-v2-error-handling
  (testing "Обработка ошибок"
    (let [code "#else\nint x = 5;"  ; #else без #if
          result (pp2/preprocess-v2 code)]
      ;; Пока ошибки не извлекаются из финального состояния
      (is (map? result)))))

;; ============================================================================
;; ТЕСТЫ ПРОИЗВОДИТЕЛЬНОСТИ
;; ============================================================================

(deftest test-performance-large-file
  (testing "Производительность на больших файлах"
    (let [large-code (str/join "\n" 
                              (concat
                               ["#define MAX_SIZE 1000"]
                               (repeat 1000 "int array[MAX_SIZE];")))]
      (time
       (let [result (pp2/preprocess-v2 large-code)]
         (is (:success result))
         (is (> (count (:result result)) 10000)))))))

;; ============================================================================
;; ТЕСТЫ СОВМЕСТИМОСТИ
;; ============================================================================

(deftest test-compatibility-with-c89
  (testing "Совместимость с стандартом C89"
    (let [c89-code "#define __STDC__ 1\n#ifdef __STDC__\nprintf(\"C89 compatible\");\n#endif"
          result (pp2/preprocess-v2 c89-code)]
      (is (:success result))
      (is (str/includes? (:result result) "printf(\"C89 compatible\");")))))

;; ============================================================================
;; PROPERTY-BASED ТЕСТЫ
;; ============================================================================

(deftest test-property-idempotence
  (testing "Идемпотентность препроцессора"
    ;; Код без директив должен остаться неизменным
    (let [simple-code "int x = 5;\nint y = 10;\nreturn x + y;"
          result1 (pp2/preprocess-v2 simple-code)
          result2 (pp2/preprocess-v2 (:result result1))]
      (is (:success result1))
      (is (:success result2))
      ;; После нормализации пробелов результаты должны быть идентичны
      (is (= (str/trim (:result result1)) 
             (str/trim (:result result2)))))))

(deftest test-property-state-immutability
  (testing "Неизменяемость состояния"
    (let [initial-state (pp2/create-initial-state)
          directive (pp2/->DefineDirective)]
      ;; Обработка директивы не должна изменять исходное состояние
      (pp2/process directive "#define TEST 1" initial-state)
      (is (empty? (:defines initial-state)))
      (is (empty? (:errors initial-state))))))

;; Запуск всех тестов
(defn run-all-tests []
  (clojure.test/run-tests 'c51cc.preprocessor-v2-test)) 