(ns c51cc.comprehensive-pipeline-test
  "Комплексная система тестирования всей цепочки компилятора C51
   
   Этот модуль реализует полное тестирование процесса компиляции:
   1. Препроцессор - обработка директив, удаление комментариев
   2. Лексер - токенизация очищенного кода  
   3. Парсер - построение AST из токенов
   4. Красивая печать AST - форматированный вывод дерева
   
   Все ошибки собираются отдельно для анализа проблемных мест."
  (:require [c51cc.preprocessor :as preprocessor]
            [c51cc.lexer :as lexer]
            [c51cc.parser :as parser]
            [c51cc.ast :as ast]
            [c51cc.core :as core]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :as pprint]))

;; ============================================================================
;; СТРУКТУРЫ ДАННЫХ ДЛЯ РЕЗУЛЬТАТОВ ТЕСТИРОВАНИЯ
;; ============================================================================

(defrecord TestResult
  [file-name           ; имя тестируемого файла
   success?            ; общий результат прохождения всех этапов
   stages              ; результаты каждого этапа
   errors              ; список ошибок
   warnings            ; список предупреждений
   execution-time      ; время выполнения в миллисекундах
   ast-output])        ; результат красивой печати AST

(defrecord StageResult
  [stage-name          ; название этапа (:preprocessor, :lexer, :parser, :ast-printer)
   success?            ; успешность этапа
   input-data          ; входные данные этапа
   output-data         ; выходные данные этапа
   error-message       ; сообщение об ошибке (если есть)
   execution-time])    ; время выполнения этапа

;; ============================================================================
;; ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
;; ============================================================================

(defn measure-time
  "Измеряет время выполнения функции и возвращает [результат время-в-мс]"
  [f]
  (let [start-time (System/nanoTime)
        result (f)
        end-time (System/nanoTime)
        execution-time (/ (- end-time start-time) 1000000.0)]
    [result execution-time]))

(defn safe-execute
  "Безопасно выполняет функцию, перехватывая исключения"
  [f error-context]
  (try
    (let [[result time] (measure-time f)]
      {:success? true
       :result result
       :error nil
       :execution-time time})
    (catch Exception e
      {:success? false
       :result nil
       :error (str error-context ": " (.getMessage e))
       :execution-time 0})))

(defn get-c-files
  "Получает список всех .c и .h файлов в указанной директории"
  [directory]
  (let [dir (io/file directory)]
    (if (.exists dir)
      (->> (.listFiles dir)
           (filter #(.isFile %))
           (filter #(or (str/ends-with? (.getName %) ".c")
                       (str/ends-with? (.getName %) ".h")))
           (map #(.getName %))
           (sort))
      [])))

;; ============================================================================
;; ЭТАПЫ ОБРАБОТКИ
;; ============================================================================

(defn run-preprocessor-stage
  "Выполняет этап препроцессора"
  [file-content file-name]
  (safe-execute
    (fn []
      (-> file-content
          preprocessor/remove-comments
          preprocessor/normalize-whitespace))
    (str "Препроцессор для файла " file-name)))

(defn run-lexer-stage
  "Выполняет этап лексического анализа"
  [preprocessed-content file-name]
  (safe-execute
    (fn []
      (lexer/tokenize preprocessed-content))
    (str "Лексер для файла " file-name)))

(defn run-parser-stage
  "Выполняет этап синтаксического анализа"
  [tokens file-name]
  (safe-execute
    (fn []
      (let [parse-state (parser/parse-state tokens)
            result (parser/parse-program parse-state)]
        (if (:success? result)
          (:value result)
          (throw (ex-info "Ошибка парсинга" {:error (:error result)})))))
    (str "Парсер для файла " file-name)))

(defn run-ast-printer-stage
  "Выполняет этап красивой печати AST"
  [ast-tree file-name]
  (safe-execute
    (fn []
      (ast/pretty-print ast-tree))
    (str "AST-принтер для файла " file-name)))

;; ============================================================================
;; ОСНОВНАЯ ЛОГИКА ТЕСТИРОВАНИЯ
;; ============================================================================

(defn process-single-file
  "Обрабатывает один файл через всю цепочку компилятора"
  [file-path file-name]
  (println (str "\n🔍 Обрабатываю файл: " file-name))
  
  (let [start-time (System/nanoTime)
        file-content (slurp file-path)
        
        ;; Этап 1: Препроцессор
        _ (print "  📝 Препроцессор... ")
        preprocessor-result (run-preprocessor-stage file-content file-name)
        _ (println (if (:success? preprocessor-result) "✅ OK" "❌ ОШИБКА"))
        
        ;; Этап 2: Лексер (только если препроцессор успешен)
        lexer-result (if (:success? preprocessor-result)
                       (do
                         (print "  🔤 Лексер... ")
                         (let [result (run-lexer-stage (:result preprocessor-result) file-name)]
                           (println (if (:success? result) "✅ OK" "❌ ОШИБКА"))
                           result))
                       {:success? false :result nil :error "Пропущен из-за ошибки препроцессора" :execution-time 0})
        
        ;; Этап 3: Парсер (только если лексер успешен)
        parser-result (if (:success? lexer-result)
                        (do
                          (print "  🌳 Парсер... ")
                          (let [result (run-parser-stage (:result lexer-result) file-name)]
                            (println (if (:success? result) "✅ OK" "❌ ОШИБКА"))
                            result))
                        {:success? false :result nil :error "Пропущен из-за ошибки лексера" :execution-time 0})
        
        ;; Этап 4: AST-принтер (только если парсер успешен)
        ast-printer-result (if (:success? parser-result)
                             (do
                               (print "  🎨 AST-принтер... ")
                               (let [result (run-ast-printer-stage (:result parser-result) file-name)]
                                 (println (if (:success? result) "✅ OK" "❌ ОШИБКА"))
                                 result))
                             {:success? false :result nil :error "Пропущен из-за ошибки парсера" :execution-time 0})
        
        ;; Подсчет общего времени
        end-time (System/nanoTime)
        total-time (/ (- end-time start-time) 1000000.0)
        
        ;; Определение общего успеха
        overall-success? (and (:success? preprocessor-result)
                             (:success? lexer-result)
                             (:success? parser-result)
                             (:success? ast-printer-result))
        
        ;; Сбор всех ошибок
        all-errors (filter some? [(:error preprocessor-result)
                                 (:error lexer-result)
                                 (:error parser-result)
                                 (:error ast-printer-result)])
        
        ;; Создание результатов этапов
        stages [(->StageResult :preprocessor (:success? preprocessor-result)
                              file-content (:result preprocessor-result)
                              (:error preprocessor-result) (:execution-time preprocessor-result))
                (->StageResult :lexer (:success? lexer-result)
                              (:result preprocessor-result) (:result lexer-result)
                              (:error lexer-result) (:execution-time lexer-result))
                (->StageResult :parser (:success? parser-result)
                              (:result lexer-result) (:result parser-result)
                              (:error parser-result) (:execution-time parser-result))
                (->StageResult :ast-printer (:success? ast-printer-result)
                              (:result parser-result) (:result ast-printer-result)
                              (:error ast-printer-result) (:execution-time ast-printer-result))]]
    
    (println (str "  ⏱️  Общее время: " (format "%.2f" total-time) " мс"))
    (println (str "  📊 Результат: " (if overall-success? "✅ УСПЕХ" "❌ ОШИБКА")))
    
    (->TestResult file-name overall-success? stages all-errors [] total-time (:result ast-printer-result))))

(defn run-comprehensive-test
  "Запускает комплексное тестирование всех файлов в директории"
  [test-directory]
  (println "🚀 Запуск комплексного тестирования цепочки компилятора C51")
  (println (str "📁 Директория тестов: " test-directory))
  (println "=" (apply str (repeat 80 "=")))
  
  (let [c-files (get-c-files test-directory)
        _ (println (str "📋 Найдено файлов для тестирования: " (count c-files)))
        
        ;; Обработка каждого файла
        test-results (doall
                       (for [file-name c-files]
                         (let [file-path (str test-directory "/" file-name)]
                           (process-single-file file-path file-name))))
        
        ;; Подсчет статистики
        successful-tests (filter :success? test-results)
        failed-tests (filter #(not (:success? %)) test-results)
        total-time (reduce + (map :execution-time test-results))]
    
    (println "\n" "=" (apply str (repeat 80 "=")))
    (println "📈 ИТОГОВАЯ СТАТИСТИКА")
    (println "=" (apply str (repeat 80 "=")))
    (println (str "✅ Успешных тестов: " (count successful-tests) "/" (count test-results)))
    (println (str "❌ Неудачных тестов: " (count failed-tests) "/" (count test-results)))
    (println (str "⏱️  Общее время выполнения: " (format "%.2f" total-time) " мс"))
    (println (str "📊 Процент успеха: " (format "%.1f" (* 100.0 (/ (count successful-tests) (count test-results)))) "%"))
    
    ;; Детальный отчет по ошибкам
    (when (seq failed-tests)
      (println "\n❌ ДЕТАЛЬНЫЙ ОТЧЕТ ПО ОШИБКАМ:")
      (println "-" (apply str (repeat 78 "-")))
      (doseq [failed-test failed-tests]
        (println (str "📄 Файл: " (:file-name failed-test)))
        (doseq [error (:errors failed-test)]
          (println (str "   💥 " error)))
        (println)))
    
    ;; Возвращаем результаты для дальнейшего анализа
    {:total-files (count test-results)
     :successful (count successful-tests)
     :failed (count failed-tests)
     :success-rate (/ (count successful-tests) (count test-results))
     :total-time total-time
     :results test-results
     :failed-results failed-tests}))

;; ============================================================================
;; УТИЛИТЫ ДЛЯ АНАЛИЗА РЕЗУЛЬТАТОВ
;; ============================================================================

(defn analyze-stage-failures
  "Анализирует, на каких этапах чаще всего происходят ошибки"
  [test-results]
  (let [stage-failures (atom {:preprocessor 0 :lexer 0 :parser 0 :ast-printer 0})]
    (doseq [result test-results]
      (when-not (:success? result)
        (doseq [stage (:stages result)]
          (when-not (:success? stage)
            (swap! stage-failures update (:stage-name stage) inc)))))
    @stage-failures))

(defn generate-detailed-report
  "Генерирует детальный отчет по результатам тестирования"
  [test-results output-file]
  (with-open [writer (io/writer output-file)]
    (.write writer "# Детальный отчет тестирования компилятора C51\n\n")
    (.write writer (str "Дата генерации: " (java.util.Date.) "\n\n"))
    
    (doseq [result test-results]
      (.write writer (str "## Файл: " (:file-name result) "\n"))
      (.write writer (str "**Результат:** " (if (:success? result) "✅ УСПЕХ" "❌ ОШИБКА") "\n"))
      (.write writer (str "**Время выполнения:** " (format "%.2f" (:execution-time result)) " мс\n\n"))
      
      (when-not (:success? result)
        (.write writer "### Ошибки:\n")
        (doseq [error (:errors result)]
          (.write writer (str "- " error "\n")))
        (.write writer "\n"))
      
      (.write writer "### Детали этапов:\n")
      (doseq [stage (:stages result)]
        (.write writer (str "- **" (name (:stage-name stage)) ":** " 
                           (if (:success? stage) "✅" "❌") 
                           " (" (format "%.2f" (:execution-time stage)) " мс)\n")))
      (.write writer "\n---\n\n"))))

;; ============================================================================
;; ТОЧКА ВХОДА ДЛЯ ЗАПУСКА ТЕСТОВ
;; ============================================================================

(defn -main
  "Точка входа для запуска комплексного тестирования"
  [& args]
  (let [test-dir (or (first args) "test/ccode")]
    (println "🎯 Комплексное тестирование компилятора C51")
    (println "Автор: Senior Developer | Комментарии на русском языке")
    (println)
    
    (let [results (run-comprehensive-test test-dir)
          stage-analysis (analyze-stage-failures (:results results))]
      
      (println "\n🔍 АНАЛИЗ ОШИБОК ПО ЭТАПАМ:")
      (println "-" (apply str (repeat 78 "-")))
      (doseq [[stage count] stage-analysis]
        (when (> count 0)
          (println (str "📊 " (name stage) ": " count " ошибок"))))
      
      ;; Генерация детального отчета
      (generate-detailed-report (:results results) "test-report.md")
      (println "\n📄 Детальный отчет сохранен в файл: test-report.md")
      
      ;; Возвращаем код выхода
      (if (> (:success-rate results) 0.8)
        (do (println "\n🎉 Тестирование завершено успешно!")
            (System/exit 0))
        (do (println "\n⚠️  Обнаружены критические проблемы!")
            (System/exit 1))))))

;; Для интерактивного запуска из REPL
(defn run-tests
  "Удобная функция для запуска тестов из REPL"
  ([]
   (run-tests "test/ccode"))
  ([test-directory]
   (run-comprehensive-test test-directory)))

(comment
  ;; Примеры использования:
  
  ;; Запуск всех тестов
  (run-tests)
  
  ;; Запуск тестов из конкретной директории
  (run-tests "c51cc/test/ccode")
  
  ;; Анализ результатов
  (let [results (run-tests)]
    (analyze-stage-failures (:results results)))
  
  ;; Генерация отчета
  (let [results (run-tests)]
    (generate-detailed-report (:results results) "my-report.md"))
  ) 