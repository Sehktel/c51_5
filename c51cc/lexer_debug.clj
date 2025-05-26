(require '[c51cc.lexer :as lexer])

(println "=== ОТЛАДКА ТОКЕНИЗАЦИИ ===")

(println "\n1. Простая функция:")
(def tokens1 (lexer/tokenize "void foo(void) { }"))
(doseq [token tokens1] (println "  " token))

(println "\n2. Только interrupt:")
(def tokens2 (lexer/tokenize "interrupt"))
(doseq [token tokens2] (println "  " token))

(println "\n3. Только число:")
(def tokens3 (lexer/tokenize "2"))
(doseq [token tokens3] (println "  " token))

(println "\n4. interrupt + число:")
(def tokens4 (lexer/tokenize "interrupt 2"))
(doseq [token tokens4] (println "  " token))

(println "\n5. Полная строка:")
(def tokens5 (lexer/tokenize "void foo(void) interrupt 2 { }"))
(println "Количество токенов:" (count tokens5))
(doseq [token tokens5] (println "  " token)) 