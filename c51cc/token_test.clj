(require '[c51cc.lexer :as lexer])

(println "Тестируем токенизацию:")
(def tokens (lexer/tokenize "void foo(void) interrupt 2 { }"))
(println "Токены:")
(doseq [token tokens]
  (println "  " token))

(println "\nТестируем с using:")
(def tokens2 (lexer/tokenize "void timer_isr(void) interrupt 1 using 2 { }"))
(println "Токены:")
(doseq [token tokens2]
  (println "  " token)) 