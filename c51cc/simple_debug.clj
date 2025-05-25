(require '[c51cc.parser :as parser])
(require '[c51cc.lexer :as lexer])

(println "Простая отладка функций")

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