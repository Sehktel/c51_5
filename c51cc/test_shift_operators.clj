(require '[c51cc.lexer :as lexer])

(println "=== ТЕСТИРОВАНИЕ ОПЕРАТОРОВ СДВИГА ===")

(defn test-tokenize [code description]
  (println (str "\n--- " description " ---"))
  (println (str "Код: " code))
  (let [tokens (lexer/tokenize code)]
    (println (str "Токенов: " (count tokens)))
    (doseq [[i token] (map-indexed vector tokens)]
      (println (str "  " i ": " token)))))

;; Тестируем различные операторы
(test-tokenize "1 << 3" "Сдвиг влево")
(test-tokenize "8 >> 2" "Сдвиг вправо")
(test-tokenize "x <<= 2" "Составной сдвиг влево")
(test-tokenize "y >>= 1" "Составной сдвиг вправо")

;; Сравнение с другими операторами
(test-tokenize "a < b" "Меньше")
(test-tokenize "a > b" "Больше")
(test-tokenize "a <= b" "Меньше или равно")
(test-tokenize "a >= b" "Больше или равно")

;; Проблемные случаи
(test-tokenize "P1 |= (1 << 3);" "Битовая операция с портом")
(test-tokenize "mask = 0xFF >> 4;" "Сдвиг константы")

(println "\n=== АНАЛИЗ ЗАВЕРШЕН ===") 