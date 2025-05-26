(ns test-c51-simple
  (:require [clojure.test :refer :all]
            [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]))

(defn parse-string [input]
  (let [tokens (lexer/tokenize input)]
    (parser/parse tokens)))

(defn parse-successful? [result]
  (:success result))

(defn get-ast [result]
  (:ast result))

(deftest test-c51-basic
  (testing "Простая interrupt функция"
    (let [result (parse-string "void timer_isr() interrupt 1 { }")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [func-decl (first (:declarations ast))]
          (is (= :interrupt-declaration (:ast-type func-decl)))
          (is (= "timer_isr" (:function-name func-decl)))
          (is (= 1 (:interrupt-number func-decl)))))))

  (testing "Interrupt функция без параметров"
    (let [result (parse-string "void serial_isr() interrupt 3 using 1 { counter++; }")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [func-decl (first (:declarations ast))]
          (is (= :interrupt-declaration (:ast-type func-decl)))
          (is (= "serial_isr" (:function-name func-decl)))
          (is (= 3 (:interrupt-number func-decl)))
          (is (= 1 (:using-clause func-decl)))))))

  (testing "SFR декларация"
    (let [result (parse-string "sfr P1 = 0x90;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [sfr-decl (first (:declarations ast))]
          (is (= :sfr-declaration (:ast-type sfr-decl)))
          (is (= "P1" (:name sfr-decl)))
          (is (= 144 (:address sfr-decl)))))))

  (testing "SBIT декларация"
    (let [result (parse-string "sbit LED = 0x97;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [sbit-decl (first (:declarations ast))]
          (is (= :sbit-declaration (:ast-type sbit-decl)))
          (is (= "LED" (:name sbit-decl)))
          (is (= 151 (:address sbit-decl))))))))

(run-tests) 