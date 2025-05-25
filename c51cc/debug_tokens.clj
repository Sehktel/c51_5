(require '[c51cc.lexer :as lexer])

(println "🔍 АНАЛИЗ ТОКЕНОВ")

(defn analyze-tokens [code]
  (println (str "\n=== " code " ==="))
  (let [tokens (lexer/tokenize code)]
    (println (str "Токенов: " (count tokens)))
    (doseq [[i token] (map-indexed vector tokens)]
      (println (str "  " i ": " (:type token) " -> " (:value token))))))

(analyze-tokens "void main() { }")
(analyze-tokens "int add(int a, int b) { return a + b; }")
(analyze-tokens "void func();") 