(ns c51cc.parser
  (:require 
   [malli.core :as m]
   [c51cc.logger :as log]
   [c51cc.config :refer [DEBUG]]
   [clojure.core.match :as match]))

;; Схема валидации токенов
(def TokenSchema 
  [:map
   [:type keyword?]
   [:value string?]
   [:line int?]
   [:column int?]])

;; Протокол парсера
(defprotocol IParser
  (parse-token [parser token])
  (is-complete? [parser])
  (get-result [parser]))

;; Мультиметоды для диспетчеризации состояний
(defmulti process-state 
  (fn [state token] 
    [state (:type token)]))

;; Записи для состояний парсера
(defrecord StructParser 
  [current-struct   ; Текущая структура
   parsing-state    ; Состояние парсинга
   parsed-structs   ; Распознанные структуры
   errors]          ; Накопленные ошибки
  
  IParser
  (parse-token [this token]
    (m/validate TokenSchema token)
    (let [new-state 
          (process-state 
           (:parsing-state this) 
           token)]
      (assoc this 
             :parsing-state (:state new-state)
             :current-struct (:current-struct new-state)
             :parsed-structs 
             (if (:complete? new-state)
               (conj (:parsed-structs this) 
                     (:current-struct new-state))
               (:parsed-structs this)))))
  
  (is-complete? [this]
    (= (:parsing-state this) :final))
  
  (get-result [this]
    {:structs (:parsed-structs this)
     :errors (:errors this)}))

;; Реализация мультиметодов для состояний
(defmethod process-state 
  [:init {:type :keyword :value "struct"}]
  [state token]
  {:state :expect-name
   :current-struct {:name nil}})

(defmethod process-state 
  [:expect-name {:type :identifier}]
  [state token]
  {:state :expect-body
   :current-struct 
   (assoc (:current-struct state) 
          :name (:value token))})

(defmethod process-state 
  [:expect-body {:type :punctuation :value "{"}]
  [state token]
  {:state :parse-fields
   :current-struct 
   (assoc (:current-struct state) 
          :fields [])})

(defmethod process-state 
  [:parse-fields {:type :identifier}]
  [state token]
  {:state :parse-fields
   :current-struct 
   (update-in 
    (:current-struct state) 
    [:fields] 
    conj 
    {:type (:value token)})})

(defmethod process-state 
  [:parse-fields {:type :punctuation :value "}"}]
  [state token]
  {:state :final
   :current-struct (:current-struct state)
   :complete? true})

;; Обработка неожиданных токенов
(defmethod process-state 
  :default 
  [state token]
  {:state :error
   :error {:unexpected-token token
           :current-state state}})

;; Главная функция парсинга
(defn parse-tokens [tokens]
  (->> tokens
       (reduce 
        (fn [parser token]
          (parse-token parser token))
        (->StructParser 
         nil     ; current-struct
         :init   ; parsing-state
         []      ; parsed-structs
         []))    ; errors
       (get-result)))

;; Расширенная схема валидации структуры
(def ExtendedStructSchema 
  [:map
   [:name string?]
   [:fields 
    [:vector 
     [:map
      [:type string?]
      [:name {:optional true} string?]]]]
   [:line {:optional true} int?]
   [:column {:optional true} int?]])

;; Пример использования
(defn example-parse []
  (let [sample-tokens 
        [{:type :keyword :value "struct"}
         {:type :identifier :value "MyStruct"}
         {:type :punctuation :value "{"}
         {:type :identifier :value "int"}
         {:type :identifier :value "x"}
         {:type :punctuation :value ";"}
         {:type :punctuation :value "}"}]
        
        parsed-result (parse-tokens sample-tokens)]
    (println "Parsed result:" parsed-result)
    (m/validate ExtendedStructSchema 
                (first (:structs parsed-result)))))

;; Точка входа для тестирования
(defn -main []
  (example-parse)) 