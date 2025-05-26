#!/usr/bin/env clj

;; Скрипт для запуска одного теста из командной строки
;; Использование: clj run_single_test.clj test_25_singleton_guard.c

(load-file "c51cc/simple_pipeline_test.clj")

(defn main [& args]
  (if (empty? args)
    (do
      (println "❌ Ошибка: Не указан файл для тестирования")
      (println "Использование: clj run_single_test.clj <имя_файла.c>")
      (println "Пример: clj run_single_test.clj test_25_singleton_guard.c")
      (System/exit 1))
    (let [filename (first args)]
      (println (str "🎯 Запуск теста для файла: " filename))
      (quick-test filename))))

;; Запуск main функции с аргументами командной строки
(apply main *command-line-args*) 