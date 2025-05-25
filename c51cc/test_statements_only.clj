(require '[clojure.test :refer :all])
(require '[c51cc.parser-test :refer :all])

;; Тестируем только операторы return
(deftest test-return-statements-only
  (testing "Оператор return без выражения"
    (let [result (parse-string "return;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [stmt (first (:declarations ast))]
          (is (= :return-statement (:ast-type stmt)))
          (is (nil? (:expression stmt)))))))

  (testing "Оператор return с выражением"
    (let [result (parse-string "return 42;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [stmt (first (:declarations ast))]
          (is (= :return-statement (:ast-type stmt)))
          (is (= 42 (:value (:expression stmt)))))))))

(run-tests 'user) 