;; Простое тестирование цепочки компилятора C51
;; Препроцессор -> Лексер -> Парсер -> Красивая печать AST

(require '[c51cc.preprocessor :as preprocessor])
(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])
(require '[c51cc.ast :as ast])
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

;; ============================================================================
;; ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
;; ============================================================================

(defn get-test-files
  "Получает список всех .c файлов в директории test/ccode"
  []
  (let [test-dir (io/file "test/ccode")]
    (if (.exists test-dir)
      (->> (.listFiles test-dir)
           (filter #(.isFile %))
           (filter #(str/ends-with? (.getName %) ".c"))
           (map #(.getName %))
           (sort))
      [])))

(defn safe-stage
  "Безопасно выполняет этап обработки, возвращая результат или ошибку"
  [stage-name f input]
  (try
    {:success? true
     :result (f input)
     :error nil}
    (catch Exception e
      {:success? false
       :result nil
       :error (str stage-name " ошибка: " (.getMessage e))})))

;; ============================================================================
;; ЭТАПЫ ОБРАБОТКИ
;; ============================================================================

(defn run-preprocessor
  "Этап 1: Препроцессор - удаление комментариев и нормализация"
  [file-content]
  (-> file-content
      preprocessor/remove-comments
      preprocessor/normalize-whitespace))

(defn run-lexer
  "Этап 2: Лексер - токенизация"
  [clean-content]
  (lexer/tokenize clean-content))

(defn run-parser
  "Этап 3: Парсер - построение AST"
  [tokens]
  (let [parse-state (parser/parse-state tokens)
        result (parser/parse-program parse-state)]
    (if (:success? result)
      (:value result)
      (throw (ex-info "Ошибка парсинга" {:error (:error result)})))))

(defn run-ast-printer
  "Этап 4: Красивая печать AST"
  [ast-tree]
  (ast/pretty-print ast-tree))

;; ============================================================================
;; ОСНОВНАЯ ФУНКЦИЯ ТЕСТИРОВАНИЯ
;; ============================================================================

(defn test-single-file
  "Тестирует один файл через всю цепочку"
  [file-name]
  (println (str "\n🔍 Тестирую файл: " file-name))
  (println (str "   " (apply str (repeat (+ 15 (count file-name)) "-"))))
  
  (let [file-path (str "test/ccode/" file-name)
        file-content (slurp file-path)]
    
    ;; Этап 1: Препроцессор
    (print "   📝 Препроцессор... ")
    (let [preprocessor-result (safe-stage "Препроцессор" run-preprocessor file-content)]
      (if (:success? preprocessor-result)
        (do
          (println "✅")
          
          ;; Этап 2: Лексер
          (print "   🔤 Лексер... ")
          (let [lexer-result (safe-stage "Лексер" run-lexer (:result preprocessor-result))]
            (if (:success? lexer-result)
              (do
                (println "✅")
                (println (str "      Токенов получено: " (count (:result lexer-result))))
                
                ;; Этап 3: Парсер
                (print "   🌳 Парсер... ")
                (let [parser-result (safe-stage "Парсер" run-parser (:result lexer-result))]
                  (if (:success? parser-result)
                    (do
                      (println "✅")
                      
                      ;; Этап 4: AST-принтер
                      (print "   🎨 AST-принтер... ")
                      (let [ast-result (safe-stage "AST-принтер" run-ast-printer (:result parser-result))]
                        (if (:success? ast-result)
                          (do
                            (println "✅")
                            (println "   📊 Результат: ✅ ПОЛНЫЙ УСПЕХ")
                            {:file file-name :success? true :errors []})
                          (do
                            (println "❌")
                            (println (str "      Ошибка: " (:error ast-result)))
                            (println "   📊 Результат: ❌ ОШИБКА НА ЭТАПЕ AST-ПРИНТЕРА")
                            {:file file-name :success? false :errors [(:error ast-result)]}))))
                    (do
                      (println "❌")
                      (println (str "      Ошибка: " (:error parser-result)))
                      (println "   📊 Результат: ❌ ОШИБКА НА ЭТАПЕ ПАРСЕРА")
                      {:file file-name :success? false :errors [(:error parser-result)]}))))
              (do
                (println "❌")
                (println (str "      Ошибка: " (:error lexer-result)))
                (println "   📊 Результат: ❌ ОШИБКА НА ЭТАПЕ ЛЕКСЕРА")
                {:file file-name :success? false :errors [(:error lexer-result)]}))))
        (do
          (println "❌")
          (println (str "      Ошибка: " (:error preprocessor-result)))
          (println "   📊 Результат: ❌ ОШИБКА НА ЭТАПЕ ПРЕПРОЦЕССОРА")
          {:file file-name :success? false :errors [(:error preprocessor-result)]})))))

(defn run-all-tests
  "Запускает тестирование всех файлов"
  []
  (println "🚀 КОМПЛЕКСНОЕ ТЕСТИРОВАНИЕ ЦЕПОЧКИ КОМПИЛЯТОРА C51")
  (println "=" (apply str (repeat 60 "=")))
  (println "Автор: Senior Developer | Комментарии на русском языке")
  (println)
  
  (let [test-files (get-test-files)]
    (if (empty? test-files)
      (println "❌ Не найдено тестовых файлов в директории test/ccode")
      (do
        (println (str "📋 Найдено файлов для тестирования: " (count test-files)))
        (println)
        
        ;; Тестируем каждый файл
        (let [results (doall (map test-single-file test-files))
              successful (filter :success? results)
              failed (filter #(not (:success? %)) results)]
          
          ;; Итоговая статистика
          (println "\n" "=" (apply str (repeat 60 "=")))
          (println "📈 ИТОГОВАЯ СТАТИСТИКА")
          (println "=" (apply str (repeat 60 "=")))
          (println (str "✅ Успешных тестов: " (count successful) "/" (count results)))
          (println (str "❌ Неудачных тестов: " (count failed) "/" (count results)))
          (println (str "📊 Процент успеха: " (format "%.1f" (* 100.0 (/ (count successful) (count results)))) "%"))
          
          ;; Детальный отчет по ошибкам
          (when (seq failed)
            (println "\n❌ ФАЙЛЫ С ОШИБКАМИ:")
            (println "-" (apply str (repeat 58 "-")))
            (doseq [failed-test failed]
              (println (str "📄 " (:file failed-test)))
              (doseq [error (:errors failed-test)]
                (println (str "   💥 " error)))
              (println)))
          
          ;; Возвращаем результаты
          {:total (count results)
           :successful (count successful)
           :failed (count failed)
           :success-rate (/ (count successful) (count results))
           :results results})))))

;; ============================================================================
;; БЫСТРЫЕ ТЕСТЫ ДЛЯ ОТЛАДКИ
;; ============================================================================

(defn quick-test
  "Быстрый тест одного файла для отладки"
  [file-name]
  (test-single-file file-name))

(defn test-simple-files
  "Тестирует только простые файлы для быстрой проверки"
  []
  (println "🎯 БЫСТРОЕ ТЕСТИРОВАНИЕ ПРОСТЫХ ФАЙЛОВ")
  (println "=" (apply str (repeat 50 "=")))
  
  (let [simple-files ["test_01_var_decl.c" "test_02_assignment.c" "test_03_arithmetic.c"]]
    (doseq [file simple-files]
      (when (.exists (io/file (str "test/ccode/" file)))
        (quick-test file)))))

;; ============================================================================
;; ЗАПУСК ТЕСТОВ
;; ============================================================================

(println "🎯 Система тестирования цепочки компилятора C51 загружена!")
(println "Доступные функции:")
(println "  (run-all-tests)     - запустить все тесты")
(println "  (test-simple-files) - быстрый тест простых файлов")
(println "  (quick-test \"file.c\") - тест одного файла")
(println) 