(ns prolog.core
  (:require [clojure.string :as str])
  (:gen-class))

(defonce occurs-check (atom true))
(defn get-occurs-check [] @occurs-check)
(defn toggle-occurs-check [] (swap! occurs-check not))

(defonce db (atom {}))

(defn variable?
  "Test if the argument is a Prolog variable.
  Prolog variables are symbols with names that start with
  a question mark, e.g. ?a, ?b, ?value."
  [x]
  (and (symbol? x) (str/starts-with? (name x) "?")))

(defn has-binding
  "Check if there is a binding for a variable."
  [bindings var-name]
  (contains? bindings var-name))

(defn lookup-binding
  "Get the value of the bound variable `var-name`"
  [bindings var-name]
  (bindings var-name))

(defn add-binding
  "Append a new binding to the list of existing bindings"
  [bindings k v]
  (assoc bindings k v))

(declare unify)

(declare occurs)

(defn unify-variable
  "Unify a variable w/ x, using, and possibly extending
   the bindings"
  [v x bindings]
  (cond (contains? bindings v)
          (unify (lookup-binding bindings v) x bindings)
        (contains? bindings x)
          (unify v (lookup-binding bindings x) bindings)
        ; fail if we force the "occurs check" and v is in x
        (and @occurs-check (occurs v x bindings)) nil
        :else (add-binding bindings v x)))

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
          (occurs v (lookup-binding bindings term) bindings)
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
          (eval-bindings bindings (lookup-binding bindings term))
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


;; Clauses are represented as lists where the first element
;; is the head and the rest is the body. Facts have an empty
;; body.

(defn clause-head
  "Returns the head of a clause."
  [clause]
  (first clause))

(defn clause-body
  "Returns the body of a clause."
  [clause]
  (rest clause))

;; We store clauses in a map of vectors, indexed by their predicate. This
;; means we store one prolog procedure per map key.

(defn kb-get-clauses
  "Returns all clauses for a predicate."
  [kb predicate]
  (kb predicate))

(defn get-functor
  "Given a predicate, return its functor, i.e. name and arity."
  [predicate]
  (symbol (str (first predicate) "/" (count (rest predicate)))))

(defn kb-add-clause
  "Adds a clause to the knowledge base."
  [kb clause]
  ; get the name of the predicate
  (let [functor (get-functor (clause-head clause))]
    (println functor)
    ; the functor must be a non-variable symbol
    (if (and (symbol? functor) (not (variable? functor)))
      ; update inside the map, creating a new vector of clauses
      ; if the functor does not exists as a key yet; return the
      ; procedure (i.e. ordered list of all clauses)
      (update-in kb [functor] (fnil conj []) clause)
      ; fail by returning nil (FIXME?)
      nil)))

(defn kb-delete-predicate
  "Deletes a predicate."
  [kb predicate]
  (dissoc kb predicate))

(defn kb-drop
  "Drop the knowledge base."
  [kb]
  (dissoc kb (keys kb)))

;; functions to use global prolog KB

(defn add-clause [clause]
  (swap! db kb-add-clause clause))

(defn delete-predicate [predicate]
  (swap! db kb-delete-predicate predicate))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
