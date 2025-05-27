(ns simple-test
  (:require [c51cc.preprocessor-simple :as pp]))

;; Тест удаления комментариев
(println "=== ТЕСТ УДАЛЕНИЯ КОММЕНТАРИЕВ ===")
(def comment-test "/* комментарий */ int x = 42; // еще комментарий")
(println "Исходный код:")
(println comment-test)
(println "После удаления комментариев:")
(println (pp/remove-comments comment-test))

;; Тест макросов
(println "\n=== ТЕСТ МАКРОСОВ ===")
(def macro-test "#define MAX 100\nint x = MAX;")
(println "Исходный код:")
(println macro-test)
(println "После препроцессирования:")
(println (pp/preprocess macro-test))

;; Тест с комментариями и макросами
(println "\n=== ТЕСТ КОММЕНТАРИИ + МАКРОСЫ ===")
(def full-test "/* комментарий */\n#define MAX 100\nint x = MAX; // комментарий")
(println "Исходный код:")
(println full-test)
(println "После препроцессирования:")
(println (pp/preprocess full-test))

;; Тест include
(println "\n=== ТЕСТ INCLUDE ===")
(def include-test "#include <reg2051.h>\n#define LED_PORT P1\nvoid main() { LED_PORT = 0xFF; }")
(println "Исходный код:")
(println include-test)
(println "После препроцессирования:")
(try
  (println (pp/preprocess include-test))
  (catch Exception e
    (println "ОШИБКА:" (.getMessage e)))) 