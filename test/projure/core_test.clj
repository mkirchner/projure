(ns projure.core-test
  (:require [clojure.test :refer :all]
            [projure.core :refer :all]))

(deftest test-binding
  (testing "binding operations"
    (let [bindings {'?a 1 '?b 2}]
      ;; existence
      (is (= true (has-binding bindings '?a)))
      (is (= true (has-binding bindings '?b)))
      (is (= false (has-binding bindings '?c)))
      ;; query
      (is (= 1 (lookup-binding bindings '?a)))
      (is (= 2 (lookup-binding bindings '?b)))
      (is (nil? (lookup-binding bindings '?c)))
      ;; add
      (is (= 3 (lookup-binding (add-binding bindings '?c 3) '?c)))
      ;; update (re-bind 5 to 3)
      (is (= 3 (lookup-binding
                (add-binding
                 (add-binding bindings '?c 5) '?c 3) '?c))))))

(deftest test-occurs-check
  (testing "Simple terms"
    (is (occurs '?x '?x {}))
    (is (not (occurs '?x '?y {})))
    (is (occurs '?x '(f ?x) {}))
    (is (occurs '?x '(?x ?y) {}))
    (is (occurs '?x '((?z ?x) ?y) {}))))

(deftest test-unification
  (testing "Unification with occurs-check"
    (is (= (unify '(?x + 1) '(2 + ?y)) {'?y 1, '?x 2}))
    (is (= (unify '?x '?y) {'?x '?y}))
    (is (= (unify '(?x ?x) '(?y ?y)) {'?x '?y}))
    (is (= (unify '(?x + 1) '(2 + ?y)) {'?y 1, '?x 2}))
    (is (= (unify '(?x ?x) '(?y ?y)) {'?x '?y}))
    (is (= (unify '(?x ?x ?x) '(?y ?y ?y)) {'?x '?y}))
    (is (= (unify '(?x ?y) '(?y ?x)) {'?x '?y}))
    (is (= (unify '(?x ?y a) '(?y ?x ?x)) {'?y 'a, '?x '?y}))
    (is (= (unify '?x '(f ?x)) fail))
    (is (= (unify '(?x ?y) '((f ?y) (f ?x))) fail))
    (is (= (unify '(?x ?y ?z) '((?y ?z) (?x ?z) (?x ?y))) fail))
    (is (= (unify 'a 'a) no-bindings)))

  (testing "Unification without occurs-check"
    (do
      (when (get-occurs-check) (toggle-occurs-check))
      (is (= (unify '?x '(f ?x)) {'?x '(f ?x)})))))

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

