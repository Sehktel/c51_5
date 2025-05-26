(ns c51cc.debug-test
  (:require [clojure.test :refer :all]
            [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]))

(deftest debug-tokenization
  "Отладочный тест для проверки токенизации"
  (testing "Простая токенизация"
    (let [tokens (lexer/tokenize "a + b")]
      (println "Результат токенизации 'a + b':" tokens)
      (is (and tokens (vector? tokens) (not-empty tokens)))))
  
  (testing "Парсинг простого выражения"
    (let [tokens (lexer/tokenize "a + b")]
      (when (and tokens (vector? tokens) (not-empty tokens))
        (let [state (parser/parse-state tokens)
              result (parser/parse-additive-expression state)]
          (println "Результат парсинга 'a + b':" result)
          (is (:success? result))))))) 