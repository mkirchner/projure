(ns prolog.core
  (:require [clojure.string :as str])
  (:gen-class))

(def occurs-check (atom true))
(defn get-occurs-check [] @occurs-check)
(defn toggle-occurs-check [] (swap! occurs-check not))

(defn variable?
  "Is the argument a prolog variable?"
  [x]
  (and (symbol? x) (str/starts-with? (name x) "?")))

(defn get-binding
  "Get the binding (key and value) for a key"
  [bindings var-name]
  (assoc {} var-name (bindings var-name)))

(defn lookup
  "Get the value of the bound variable `var-name`"
  [bindings var-name]
  (bindings var-name))

(defn extend-bindings
  "Append a new binding to the list of existing bindings"
  [bindings k v]
  (assoc bindings k v))

(defn match-variable
  "Match a variable: check that the variable value corresponds
   to the current binding or update the bindings to include
   the new definition."
  [bindings var-name input]
  (let [value (lookup bindings var-name)]
   (cond (not value) (extend-bindings bindings var-name input)
         (= value input) bindings
         :else nil)))

(declare unify)
(declare occurs)

(defn unify-variable
  "Unify a variable w/ x, using, and possibly extending
   the bindings"
  [v x bindings]
  (cond (contains? bindings v) (unify (lookup bindings v) x bindings)
        (contains? bindings x) (unify v (lookup bindings x) bindings)
        ; fail if we force the "occurs check" and v is in x
        (and @occurs-check (occurs v x bindings)) nil
        :else (extend-bindings bindings v x)))

(defn unify
  ""
  ([x y] (unify x y {}))
  ([x y bindings]
   (cond (not bindings) nil
         (= x y) bindings
         (variable? x) (unify-variable x y bindings)
         (variable? y) (unify-variable y x bindings)
         (and (seq? x) (seq? y))
           (unify (rest x) (rest y)
                  (unify (first x) (first y) bindings))
         :else nil)))

(defn occurs
  "Determine if a variable occurs in a term, recursively resolving
  bound variables"
  [v term bindings]
  (cond ; var and term are equal
        (= v term) true
        ; the term is a bound variable and we need to resolve
        (and (variable? term) (contains? bindings term))
          (occurs v (lookup bindings term) bindings)
        ; the term is a sequence of terms and we need to
        ; recurse over the sequence
        (and (seq? term) (not (empty? term)))
          (or (occurs v (first term) bindings)
              (occurs v (rest term) bindings))
        ; nothing found
        :else false))

(defn eval-bindings
  ""
  [bindings term]
  (cond ; invalid bindings
        (= bindings nil) nil
        ; no bindings
        (= bindings {}) term
        ; term is a bound variable, recurse on its value
        (and (variable? term) (contains? bindings term))
          (eval-bindings bindings (lookup bindings term))
        ; term is a sequence, recurse on elements of the sequence
        (and (seq? term) (not (empty? term)))
          (cons (eval-bindings bindings (first term))
                (eval-bindings bindings (rest term)))
        ; term is not a bound variable and not a sequence, return
        :else term))

(defn unifier
  ""
  [x y]
  (eval-bindings (unify x y) x))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
