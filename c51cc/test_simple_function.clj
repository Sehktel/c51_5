(require '[c51cc.parser :as parser])
(require '[c51cc.lexer :as lexer])

(defn parse-string [input]
  (let [tokens (lexer/tokenize input)]
    (parser/parse tokens)))

(defn test-function [code]
  (println (str "\n=== Тест: " code " ==="))
  (let [result (parse-string code)]
    (println (str "Успех: " (:success result)))
    (when (:success result)
      (let [ast (:ast result)]
        (println (str "AST тип: " (:ast-type ast)))
        (println (str "Объявлений: " (count (:declarations ast))))
        (doseq [[i decl] (map-indexed vector (:declarations ast))]
          (println (str "  " i ": " (if decl (:ast-type decl) "nil"))))))))

;; Тестируем разные функции
(test-function "void main() { }")
(test-function "int x;")
(test-function "int add(int a, int b) { return a + b; }")
(test-function "void func();")  ; Объявление без тела 