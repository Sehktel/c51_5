(ns test-for-comprehensive
  (:require [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]))

(defn test-for-case [description input]
  (println (str "\n=== " description " ==="))
  (println "Input:" input)
  (let [tokens (lexer/tokenize input)
        result (parser/parse tokens)]
    (println "Parse successful?:" (:success result))
    (if (:success result)
      (let [declarations (:declarations (:ast result))]
        (println "Declarations count:" (count declarations))
        (when-let [first-decl (first declarations)]
          (println "First declaration type:" (:ast-type first-decl))
          (when (= (:ast-type first-decl) :for-statement)
            (println "Init:" (:init first-decl))
            (println "Condition:" (:condition first-decl))
            (println "Update:" (:update first-decl))
            (println "Body type:" (:ast-type (:body first-decl))))))
      (println "Error:" (:error result)))))

(defn -main []
  (println "=== КОМПЛЕКСНОЕ ТЕСТИРОВАНИЕ FOR-ЦИКЛОВ ===")
  
  ;; Базовый for с +=
  (test-for-case "Базовый for с +=" 
                 "for (i = 0; i < 10; i++) sum += i;")
  
  ;; For с -=
  (test-for-case "For с -=" 
                 "for (i = 10; i > 0; i--) sum -= i;")
  
  ;; For с блоком
  (test-for-case "For с блоком" 
                 "for (i = 0; i < 10; i++) { sum += i; count++; }")
  
  ;; For с объявлением переменной в инициализации
  (test-for-case "For с объявлением переменной" 
                 "for (int i = 0; i < 10; i++) sum += i;")
  
  ;; For без инициализации
  (test-for-case "For без инициализации" 
                 "for (; i < 10; i++) sum += i;")
  
  ;; For без условия
  (test-for-case "For без условия" 
                 "for (i = 0; ; i++) sum += i;")
  
  ;; For без обновления
  (test-for-case "For без обновления" 
                 "for (i = 0; i < 10; ) sum += i;")
  
  ;; Пустой for
  (test-for-case "Пустой for" 
                 "for (;;) sum += i;"))

(-main) 