(require 'c51cc.preprocessor)
(println "Preprocessor loaded successfully!")

;; Простой тест
(let [result (c51cc.preprocessor/preprocess "#define MAX 100\nint x = MAX;")]
  (println "Test result:")
  (println (:result result))
  (println "Success:" (:success result))
  (println "Errors:" (:errors result))) 