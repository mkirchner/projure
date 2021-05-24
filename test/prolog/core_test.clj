(ns prolog.core-test
  (:require [clojure.test :refer :all]
            [prolog.core :refer :all]))

; TODO: re-add the binding tests

(deftest test-variable-matching
  (testing "Match a variable"
    (let [bindings {'?x 42}]
      (is (contains? (match-variable bindings '?a 1) '?a)
         "Add new binding")
      (is (= ((match-variable bindings '?a 1) '?a) 1)
         "New binding incorrect")
      (is (= (match-variable bindings '?x 42) bindings) "Incorrect match")
      (is (= (match-variable bindings '?x 7) nil) "Failed to fail"))))

(deftest test-unification
 (testing "Without occurs-check"
    (do
      (when (get-occurs-check) (toggle-occurs-check))
      (is (= (unify '?x '(f ?x)) {'?x '(f ?x)})))))

(deftest test-occurs-check
  (testing "Simple terms"
    (is (occurs '?x '?x {}))
    (is (not (occurs '?x '?y {})))
    (is (occurs '?x '(f ?x) {}))
    (is (occurs '?x '(?x ?y) {}))
    (is (occurs '?x '((?z ?x) ?y) {}))))

(deftest test-binding-eval
  (testing "Basic substitution/eval"
    (is (= (eval-bindings {'?a 5} '?a) 5))
    (is (= (eval-bindings {'?a 5} '(?a)) '(5)))
    (is (= (eval-bindings {'?a 5 '?b 7} '?a) 5))
    (is (= (eval-bindings {'?a 5 '?b 7} '(?a ?b ?c)) '(5 7 ?c)))))

(deftest test-unifier
  (testing "Basic unification"
    (is (= (unifier '(?x ?y a) '(?y ?x ?x)) '(a a a)))
    (is (= (unifier '((?a * ?x + 2) + (?b * ?x) + ?c)
                    '(?z + (4 * 5) + 3))
           '((?a * 5 + 2) + (4 * 5) + 3)))))
  
