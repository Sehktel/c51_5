(ns c51cc.preprocessor-simple
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; Простое состояние препроцессора
(def initial-state
  {:defines {}
   :include-paths ["." "include" "test/ccode"]
   :conditional-stack []})

(defn remove-comments
  "Удаляет комментарии из кода"
  [code]
  (-> code
      ;; Удаляем многострочные комментарии /* */
      (str/replace #"/\*(?s).*?\*/" " ")
      ;; Удаляем однострочные комментарии //
      (str/replace #"//.*?(?:\r\n|\r|\n|$)" "\n")))

(defn expand-macros
  "Простое расширение макросов"
  [text defines]
  (reduce (fn [result [macro-name macro-value]]
            (str/replace result 
                        (re-pattern (str "\\b" macro-name "\\b"))
                        (str macro-value)))
          text
          defines))

(defn process-include
  "Обрабатывает #include"
  [filename state]
  (let [include-paths (:include-paths state)]
    (loop [paths include-paths]
      (if (empty? paths)
        (throw (ex-info (str "Файл не найден: " filename) {:filename filename}))
        (let [full-path (if (= (first paths) ".")
                          filename
                          (str (first paths) "/" filename))
              file (io/file full-path)]
          (if (.exists file)
            (slurp file)
            (recur (rest paths))))))))

(defn process-line
  "Обрабатывает одну строку"
  [line state]
  (let [trimmed (str/trim line)]
    (cond
      ;; Пустая строка
      (str/blank? trimmed)
      [line state]
      
      ;; #include
      (re-find #"^\s*#\s*include\s*[<\"]([^>\"]+)[>\"]" trimmed)
      (let [[_ filename] (re-find #"^\s*#\s*include\s*[<\"]([^>\"]+)[>\"]" trimmed)]
        (try
          (let [content (process-include filename state)]
            [content state])
          (catch Exception e
            [(str "// ОШИБКА: " (.getMessage e)) state])))
      
      ;; #define
      (re-find #"^\s*#\s*define\s+(\w+)(?:\s+(.*))?$" trimmed)
      (let [[_ macro-name macro-value] (re-find #"^\s*#\s*define\s+(\w+)(?:\s+(.*))?$" trimmed)
            new-state (assoc-in state [:defines macro-name] (or macro-value ""))]
        ["" new-state])
      
      ;; #ifdef
      (re-find #"^\s*#\s*ifdef\s+(\w+)" trimmed)
      (let [[_ macro-name] (re-find #"^\s*#\s*ifdef\s+(\w+)" trimmed)
            condition (contains? (:defines state) macro-name)
            new-state (update state :conditional-stack conj {:active condition})]
        ["" new-state])
      
      ;; #ifndef
      (re-find #"^\s*#\s*ifndef\s+(\w+)" trimmed)
      (let [[_ macro-name] (re-find #"^\s*#\s*ifndef\s+(\w+)" trimmed)
            condition (not (contains? (:defines state) macro-name))
            new-state (update state :conditional-stack conj {:active condition})]
        ["" new-state])
      
      ;; #endif
      (re-find #"^\s*#\s*endif" trimmed)
      (let [new-state (update state :conditional-stack pop)]
        ["" new-state])
      
      ;; Обычная строка
      :else
      (let [should-include? (or (empty? (:conditional-stack state))
                               (every? :active (:conditional-stack state)))]
        (if should-include?
          (let [expanded (expand-macros line (:defines state))]
            [expanded state])
          ["" state])))))

(defn preprocess
  "Основная функция препроцессора"
  ([code] (preprocess code {}))
  ([code options]
   (let [;; Удаляем комментарии
         code-clean (remove-comments code)
         ;; Начальное состояние
         state (merge initial-state options)
         lines (str/split-lines code-clean)]
     (loop [remaining-lines lines
            result-lines []
            current-state state]
       (if (empty? remaining-lines)
         (str/join "\n" result-lines)
         (let [line (first remaining-lines)
               [processed-line new-state] (process-line line current-state)]
           (recur (rest remaining-lines)
                  (if (str/blank? processed-line)
                    result-lines
                    (conj result-lines processed-line))
                  new-state)))))))

;; Алиас для совместимости
(def remove-whitespace identity) 