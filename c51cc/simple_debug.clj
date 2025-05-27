(require '[c51cc.parser :as parser])
(require '[c51cc.lexer :as lexer])
(require '[c51cc.preprocessor :as pp])
(require '[clojure.string :as str])

(println "=== Простая отладка ===")

;; Тестируем пошагово
(println "\n1. Только #define DEBUG:")
(let [result (pp/preprocess "#define DEBUG")]
  (println "Результат:" (pr-str result)))

(println "\n2. #define DEBUG + #ifdef DEBUG:")
(let [result (pp/preprocess "#define DEBUG\n#ifdef DEBUG\ntest_line\n#endif")]
  (println "Результат:" (pr-str result)))

(println "\n3. #define DEBUG + #ifdef DEBUG + #define LED:")
(let [result (pp/preprocess "#define DEBUG\n#ifdef DEBUG\n#define LED P1_7\n#endif")]
  (println "Результат:" (pr-str result)))

(println "\n4. Полный тест с #else:")
(let [result (pp/preprocess "#define DEBUG\n#ifdef DEBUG\n#define LED P1_7\n#else\n#define LED P1_0\n#endif")]
  (println "Результат:" (pr-str result)))

(println "\n5. Полный тест с использованием LED:")
(let [result (pp/preprocess "#define DEBUG\n#ifdef DEBUG\n#define LED P1_7\n#else\n#define LED P1_0\n#endif\nLED = 1;")]
  (println "Результат:" (pr-str result))
  (println "Содержит P1_7:" (str/includes? result "P1_7"))
  (println "Содержит P1_0:" (str/includes? result "P1_0")))

(println "\n=== Отладка завершена ===")

(let [code "void main() { }"
      tokens (lexer/tokenize code)
      result (parser/parse tokens)]
  (println (str "Код: " code))
  (println (str "Токенов: " (count tokens)))
  (println (str "Успех: " (:success result)))
  (when (:success result)
    (let [ast (:ast result)]
      (println (str "AST: " ast))
      (println (str "Declarations: " (:declarations ast)))
      (println (str "Первое объявление: " (first (:declarations ast))))))) 