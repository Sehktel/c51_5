(ns test-preprocessor
  (:require [c51cc.preprocessor :as pp]
            [clojure.string :as str]))

;; Тестовые примеры для демонстрации работы препроцессора

(defn test-simple-macros []
  "Тестирует простые макросы #define"
  (println "=== Тест простых макросов ===")
  (let [code "#define MAX_SIZE 100
#define PI 3.14159
#define GREETING \"Hello, World!\"

int array[MAX_SIZE];
float radius = PI * 2;
char* message = GREETING;"
        result (pp/preprocess code)]
    (println "Исходный код:")
    (println code)
    (println "\nРезультат после препроцессирования:")
    (println result)
    (println)))

(defn test-conditional-compilation []
  "Тестирует условную компиляцию #ifdef, #ifndef"
  (println "=== Тест условной компиляции ===")
  (let [code "#define DEBUG 1

#ifdef DEBUG
    printf(\"Debug mode enabled\");
#else
    printf(\"Release mode\");
#endif

#ifndef RELEASE
    int debug_flag = 1;
#endif"
        result (pp/preprocess code)]
    (println "Исходный код:")
    (println code)
    (println "\nРезультат после препроцессирования:")
    (println result)
    (println)))

(defn test-include-guards []
  "Тестирует создание защитных макросов для заголовочных файлов"
  (println "=== Тест защитных макросов ===")
  (let [header-content "typedef struct {
    int x, y;
} Point;

void draw_point(Point* p);"
        wrapped-header (pp/wrap-with-include-guard header-content "graphics.h")]
    (println "Содержимое заголовочного файла:")
    (println header-content)
    (println "\nС защитными макросами:")
    (println wrapped-header)
    (println)))

(defn test-functional-macros []
  "Тестирует функциональные макросы с параметрами"
  (println "=== Тест функциональных макросов ===")
  (let [code "#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define SQUARE(x) ((x) * (x))
#define PRINT_VAR(var) printf(#var \" = %d\\n\", var)

int max_val = MAX(10, 20);
int squared = SQUARE(5);"
        result (pp/preprocess code)]
    (println "Исходный код:")
    (println code)
    (println "\nРезультат после препроцессирования:")
    (println result)
    (println)))

(defn test-predefined-macros []
  "Тестирует предопределенные макросы __LINE__, __FILE__, etc."
  (println "=== Тест предопределенных макросов ===")
  (let [code "printf(\"Файл: %s, строка: %d\\n\", __FILE__, __LINE__);
printf(\"Дата компиляции: %s\\n\", __DATE__);
printf(\"Время компиляции: %s\\n\", __TIME__);"
        result (pp/preprocess code {:current-file "test.c"})]
    (println "Исходный код:")
    (println code)
    (println "\nРезультат после препроцессирования:")
    (println result)
    (println)))

(defn test-complex-conditions []
  "Тестирует сложные условия препроцессора"
  (println "=== Тест сложных условий ===")
  (let [code "#define VERSION 2

#if VERSION >= 2
    #define NEW_FEATURE_ENABLED
    printf(\"Новая функциональность доступна\");
#else
    printf(\"Старая версия\");
#endif

#ifdef NEW_FEATURE_ENABLED
    void new_function() {
        printf(\"Новая функция\");
    }
#endif"
        result (pp/preprocess code)]
    (println "Исходный код:")
    (println code)
    (println "\nРезультат после препроцессирования:")
    (println result)
    (println)))

(defn test-error-handling []
  "Тестирует обработку ошибок препроцессора"
  (println "=== Тест обработки ошибок ===")
  (try
    (let [code "#ifdef DEBUG
    printf(\"Debug\");
// Отсутствует #endif - должна быть ошибка"
          result (pp/preprocess code)]
      (println "Неожиданно успешно обработано!"))
    (catch Exception e
      (println "Ожидаемая ошибка:" (.getMessage e))))
  (println))

(defn run-all-tests []
  "Запускает все тесты препроцессора"
  (println "ТЕСТИРОВАНИЕ ПРЕПРОЦЕССОРА C51")
  (println "==============================")
  (test-simple-macros)
  (test-conditional-compilation)
  (test-include-guards)
  (test-functional-macros)
  (test-predefined-macros)
  (test-complex-conditions)
  (test-error-handling)
  (println "Все тесты завершены!"))

;; Запуск тестов при загрузке файла
(when (= *file* "test_preprocessor.clj")
  (run-all-tests)) 