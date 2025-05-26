#!/usr/bin/env clojure

;; Простой скрипт для запуска комплексного тестирования компилятора C51
;; Использование: clojure run_pipeline_tests.clj [директория]

(require '[c51cc.comprehensive-pipeline-test :as test])

(defn -main [& args]
  (let [test-dir (or (first args) "test/ccode")]
    (println "🎯 Быстрый запуск тестирования компилятора C51")
    (println (str "📁 Тестируемая директория: " test-dir))
    (println)
    
    ;; Запускаем тесты
    (test/run-tests test-dir)))

;; Если скрипт запущен напрямую
(when (= *file* (first *command-line-args*))
  (apply -main (rest *command-line-args*))) 