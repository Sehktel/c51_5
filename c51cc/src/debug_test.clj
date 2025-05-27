(ns debug-test
  (:require [c51cc.preprocessor :as pp]))

;; Простой тест для отладки
(def test-code "#include <reg2051.h>\n#define LED_PORT P1\nvoid main() { LED_PORT = 0xFF; }")

(println "Тестируем код:")
(println test-code)
(println "\nРезультат:")
(println (pp/preprocess test-code {:include-paths ["test/ccode" "include"]}))

;; Еще более простой тест
(def simple-test "#define LED_PORT P1\nvoid main() { LED_PORT = 0xFF; }")
(println "\n\nПростой тест:")
(println simple-test)
(println "\nРезультат:")
(println (pp/preprocess simple-test))

;; Тест только расширения макросов
(def macro-test "#define TEST 42\nint x = TEST;")
(println "Тест макросов:")
(println macro-test)
(println "Результат:")
(println (pp/preprocess macro-test))

;; Тест с LED_PORT без include
(def led-test "#define LED_PORT P1\nvoid main() { LED_PORT = 0xFF; }")
(println "\n\nТест LED_PORT без include:")
(println led-test)
(println "Результат:")
(println (pp/preprocess led-test))

;; Тест с include + define + использование
(def full-test "#include <reg2051.h>\n#define LED_PORT P1\nvoid main() { LED_PORT = 0xFF; }")
(println "\n\nПолный тест:")
(println full-test)
(println "Результат:")
(let [result (pp/preprocess full-test {:include-paths ["test/ccode" "include"]})]
  (println result)
  (println "\n=== АНАЛИЗ ===")
  (println "Содержит 'LED_PORT':" (boolean (re-find #"LED_PORT" result)))
  (println "Содержит 'P1 = 0xFF':" (boolean (re-find #"P1 = 0xFF" result)))
  (println "Содержит 'LED_PORT = 0xFF':" (boolean (re-find #"LED_PORT = 0xFF" result))))

;; Тест удаления комментариев
(def comment-test "/* комментарий */ int x = 42; // еще комментарий")
(println "Тест комментариев:")
(println comment-test)
(println "Результат:")
(println (pp/remove-comments comment-test))

;; Тест полного препроцессора с комментариями
(def full-comment-test "/* комментарий */\n#define MAX 100\nint x = MAX; // комментарий")
(println "\n\nПолный тест с комментариями:")
(println full-comment-test)
(println "Результат:")
(try
  (println (pp/preprocess full-comment-test))
  (catch Exception e
    (println "ОШИБКА:" (.getMessage e)))) 