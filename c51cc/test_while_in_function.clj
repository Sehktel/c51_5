(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])

(println "=== ТЕСТ WHILE В НАЧАЛЕ ТЕЛА ФУНКЦИИ ===")
(def while-first "void test() { while (1) { } }")
(let [result (parser/parse (lexer/tokenize while-first))]
  (if (:success result)
    (println "✅ УСПЕХ! While в начале тела функции парсится")
    (do
      (println "❌ ОШИБКА!")
      (println "Позиция:" (:position result))
      (println "Ошибка:" (:error result)))))

(println "\n=== ТЕСТ WHILE С ПЕРЕМЕННЫМИ ПЕРЕД НИМ ===")
(def vars-then-while "void test() { int i; while (i < 10) { i++; } }")
(let [result (parser/parse (lexer/tokenize vars-then-while))]
  (if (:success result)
    (println "✅ УСПЕХ! Переменные + while парсится")
    (do
      (println "❌ ОШИБКА!")
      (println "Позиция:" (:position result))
      (println "Ошибка:" (:error result)))))

(println "\n=== ТЕСТ WHILE С МНОЖЕСТВЕННЫМИ ПЕРЕМЕННЫМИ ===")
(def multi-vars-while "void test() { unsigned int i, j; while (i < j) { i++; } }")
(let [result (parser/parse (lexer/tokenize multi-vars-while))]
  (if (:success result)
    (println "✅ УСПЕХ! Множественные переменные + while парсится")
    (do
      (println "❌ ОШИБКА!")
      (println "Позиция:" (:position result))
      (println "Ошибка:" (:error result)))))

(println "\n=== ТЕСТ ФУНКЦИИ С USING И WHILE ===")
(def using-while "void delay(unsigned int ms) using 1 { while (ms > 0) { ms--; } }")
(let [result (parser/parse (lexer/tokenize using-while))]
  (if (:success result)
    (println "✅ УСПЕХ! Функция с using + while парсится")
    (do
      (println "❌ ОШИБКА!")
      (println "Позиция:" (:position result))
      (println "Ошибка:" (:error result)))))

(println "\n=== ТЕСТ ПОЛНОЙ ФУНКЦИИ DELAY С WHILE ВМЕСТО FOR ===")
(def delay-with-while "void delay(unsigned int ms) using 1 { 
  unsigned int i, j; 
  while (i < ms) { 
    j = 0;
    while (j < 1000) { 
      j++; 
    }
    i++; 
  } 
}")
(let [result (parser/parse (lexer/tokenize delay-with-while))]
  (if (:success result)
    (println "✅ УСПЕХ! Полная функция delay с while парсится")
    (do
      (println "❌ ОШИБКА!")
      (println "Позиция:" (:position result))
      (println "Ошибка:" (:error result))))) 