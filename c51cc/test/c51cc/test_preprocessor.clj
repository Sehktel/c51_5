(ns c51cc.test-preprocessor
  (:require [clojure.test :refer :all]
            [c51cc.preprocessor :as pp]
            [clojure.string :as str]))

;; Тестовые примеры для демонстрации работы препроцессора

(deftest test-simple-macros
  "Тестирует простые макросы #define"
  (testing "Простые макросы"
    (let [code "#define MAX_SIZE 100\n#define PI 3.14159\nint array[MAX_SIZE];\nfloat radius = PI * 2;"
          result (pp/preprocess code)]
      (is (str/includes? result "int array[100];"))
      (is (str/includes? result "float radius = 3.14159 * 2;")))))

(deftest test-conditional-compilation
  "Тестирует условную компиляцию #ifdef, #ifndef"
  (testing "Условная компиляция ifdef"
    (let [code "#define DEBUG 1\n#ifdef DEBUG\nprintf(\"Debug mode\");\n#endif"
          result (pp/preprocess code)]
      (is (str/includes? result "printf(\"Debug mode\");"))
      (is (not (str/includes? result "#ifdef")))))
  
  (testing "Условная компиляция ifndef"
    (let [code "#ifndef RELEASE\nint debug_flag = 1;\n#endif"
          result (pp/preprocess code)]
      (is (str/includes? result "int debug_flag = 1;")))))

(deftest test-include-guards
  "Тестирует создание защитных макросов для заголовочных файлов"
  (testing "Генерация include guard"
    (let [guard (pp/generate-include-guard "graphics.h")]
      (is (= guard "__GRAPHICS_H_H__"))))
  
  (testing "Обертывание в include guard"
    (let [content "typedef struct { int x, y; } Point;"
          wrapped (pp/wrap-with-include-guard content "test.h")]
      (is (str/includes? wrapped "#ifndef __TEST_H_H__"))
      (is (str/includes? wrapped "#define __TEST_H_H__"))
      (is (str/includes? wrapped "#endif")))))

(deftest test-predefined-macros
  "Тестирует предопределенные макросы __LINE__, __FILE__, etc."
  (testing "Предопределенные макросы"
    (let [code "printf(\"%s:%d\", __FILE__, __LINE__);"
          result (pp/preprocess code {:current-file "test.c"})]
      (is (str/includes? result "\"test.c\""))
      (is (str/includes? result "1")))))

(deftest test-macro-expansion
  "Тестирует расширение макросов"
  (testing "Базовое расширение макросов"
    (let [code "#define BUFFER_SIZE 256\nchar buffer[BUFFER_SIZE];"
          result (pp/preprocess code)]
      (is (str/includes? result "char buffer[256];")))))

(deftest test-error-handling
  "Тестирует обработку ошибок препроцессора"
  (testing "Незакрытый условный блок"
    (is (thrown? Exception 
                 (pp/preprocess "#ifdef DEBUG\nprintf(\"Debug\");\n"))))
  
  (testing "Неправильный тип входных данных"
    (is (thrown? Exception (pp/preprocess 123)))
    (is (thrown? Exception (pp/preprocess nil "not-a-map")))))

(deftest test-nested-conditions
  "Тестирует вложенные условные блоки"
  (testing "Вложенные ifdef"
    (let [code "#define DEBUG\n#define VERBOSE\n#ifdef DEBUG\n#ifdef VERBOSE\nprintf(\"Verbose debug\");\n#endif\n#endif"
          result (pp/preprocess code)]
      (is (str/includes? result "printf(\"Verbose debug\");")))))

(deftest test-undef-directive
  "Тестирует директиву #undef"
  (testing "Отмена определения макроса"
    (let [code "#define TEST_MACRO 1\n#undef TEST_MACRO\n#ifdef TEST_MACRO\nshould_not_appear\n#endif"
          result (pp/preprocess code)]
      (is (not (str/includes? result "should_not_appear"))))))

(deftest test-macro-with-spaces
  "Тестирует макросы с пробелами в определении"
  (testing "Макрос с пробелами"
    (let [code "#define GREETING \"Hello, World!\"\nprintf(GREETING);"
          result (pp/preprocess code)]
      (is (str/includes? result "printf(\"Hello, World!\");")))))

(deftest test-empty-macro
  "Тестирует пустые макросы"
  (testing "Пустой макрос"
    (let [code "#define EMPTY\n#ifdef EMPTY\ncode_here\n#endif"
          result (pp/preprocess code)]
      (is (str/includes? result "code_here")))))

;; Функция для запуска всех тестов с выводом
(defn run-all-tests-with-output []
  "Запускает все тесты с подробным выводом"
  (println "=== ЗАПУСК ТЕСТОВ ПРЕПРОЦЕССОРА ===")
  (let [results (run-tests 'c51cc.test-preprocessor)]
    (println "\n=== РЕЗУЛЬТАТЫ ТЕСТОВ ===")
    (println "Тестов выполнено:" (:test results))
    (println "Проверок выполнено:" (:pass results))
    (println "Ошибок:" (:fail results))
    (println "Исключений:" (:error results))
    results))

;; Запуск тестов при загрузке файла
(when (= *file* "test_preprocessor.clj")
  (run-all-tests-with-output)) 