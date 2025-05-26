(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])

(println "=== ОТЛАДКА VOID ПАРАМЕТРОВ ===")

;; Тест 1: Функция без параметров
(let [code "void test() { }"
      tokens (lexer/tokenize code)
      result (parser/parse tokens)]
  (println "\n1. Функция без параметров:")
  (println "Код:" code)
  (println "Успех:" (:success result))
  (when (:success result)
    (println "AST:" (:ast result))))

;; Тест 2: Функция с void параметром
(let [code "void test(void) { }"
      tokens (lexer/tokenize code)
      result (parser/parse tokens)]
  (println "\n2. Функция с void параметром:")
  (println "Код:" code)
  (println "Успех:" (:success result))
  (if (:success result)
    (println "AST:" (:ast result))
    (println "Ошибка:" (:error result))))

;; Тест 3: Функция с обычными параметрами
(let [code "void test(int x) { }"
      tokens (lexer/tokenize code)
      result (parser/parse tokens)]
  (println "\n3. Функция с int параметром:")
  (println "Код:" code)
  (println "Успех:" (:success result))
  (when (:success result)
    (println "AST:" (:ast result))))

;; Тест 4: Токенизация void
(let [tokens (lexer/tokenize "void")]
  (println "\n4. Токенизация 'void':")
  (doseq [token tokens] (println "  " token))) 