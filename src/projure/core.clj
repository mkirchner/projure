(ns projure.core
  (:require [clojure.string :as str]))

(defonce occurs-check (atom true))
(defn get-occurs-check [] @occurs-check)
(defn toggle-occurs-check [] (swap! occurs-check not))

(defonce db (atom {}))

(def fail "Indicate failure in the return value" nil)
(defn fail? [x] (nil? x))

(def no-bindings "Indicate that unification was successful
                          but there were no bindings" {})
(defn no-bindings? [x] (and (map? x) (empty? x)))

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
        (and @occurs-check (occurs v x bindings)) fail
        :else (add-binding bindings v x)))

(defn unify
  ""
  ([x y] (unify x y {}))
  ([x y bindings]
   (cond (fail? bindings) fail
         (= x y) bindings
         (variable? x) (unify-variable x y bindings)
         (variable? y) (unify-variable y x bindings)
         (and (sequential? x) (sequential? y))
           (unify (rest x) (rest y)
                  (unify (first x) (first y) bindings))
         :else fail)))

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
        (fail? bindings) fail
        ; no bindings
        (no-bindings? bindings) term
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
    ; the functor must be a non-variable symbol
    (if (and (symbol? functor) (not (variable? functor)))
      ; update inside the map, creating a new vector of clauses
      ; if the functor does not exists as a key yet; return the
      ; procedure (i.e. ordered list of all clauses)
      (update-in kb [functor] (fnil conj []) clause)
      ; fail by returning nil
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

(defn get-clauses [functor]
  (kb-get-clauses @db functor))

(defmacro <-
  "Add a clause to the Prolog knowledge base.
  This allows spelling out clauses w/o the quoting that would be required
  in raw Clojure."
  [& clauses]
  `(add-clause '~(vec clauses)))

;; prove

(defn find-unique-leaves-if
  "Find and (uniquely) collect all leaves of a tree that satisfy
  a predicate"
  ([predicate tree]
  (find-unique-leaves-if predicate tree #{}))
  ([predicate tree acc]
  (cond ; tree is a leaf
        (not (sequential? tree))
          (if (predicate tree) (conj acc tree) acc)
        ; tree is a list, recurse over content
        (and (sequential? tree) (not (empty? tree)))
          (find-unique-leaves-if
            predicate
            (first tree)
            (find-unique-leaves-if predicate (rest tree) acc))
        ; empty list
        :else acc)))

(defn variables
  "Return a list of all variables in an expression."
  [expr]
  (find-unique-leaves-if variable? expr))

;; replace substitue w/ clojure.walk/prewalk-replace ??
(defn substitute
  ; allows functions or maps as sfn
  [sset sfn tree]
  (cond ; tree is a leaf
        (not (sequential? tree)) (if (contains? sset tree)
                                   (sfn tree)
                                   tree)
        ; tree is not a leaf and not empty
        (and (sequential? tree) (not (empty? tree)))
          (conj (substitute sset sfn (rest tree))
                (substitute sset sfn (first tree)))
        ; fixpoint
        :else (list)))

(defn rename-variables
  "Replace all prolog variables in expression w/ unique gensyms"
  [expr]
  (let [vars (variables expr)
        varmap (zipmap vars (map #(gensym (str %1)) vars))]
    (substitute vars varmap expr)))

(declare prove)
(declare prove-all)
(declare prove-clause)

;; notes: all prove-* functions return a list of bindings (i.e. solutions)

(defn prove-clause
  "Prove a clause (renaming its variables)"
  [goal bindings clause]
  (let [new-clause (vec (rename-variables clause))
        solution   (unify goal (clause-head new-clause) bindings)]
    (prove-all (clause-body new-clause) solution)))

(defn prove
  "Prove a single goal, returning a list of possible solutions"
  [goal bindings]
  (flatten
    (filter
      identity
      (map (partial prove-clause goal bindings)
           (get-clauses (get-functor goal))))))

(defn prove-all
  "Return a list of solutions to a conjunction of goals"
  [goals bindings]
  (cond ; no bindings
        (fail? bindings) fail
        ; no goals left, return the bindings
        (empty? goals) bindings
        ; else recurse on the list
        :else (let [cur-solution (prove (first goals) bindings)]
                ; filter all nil values and flatten nested solutions
                (flatten (filter identity (map (partial prove-all (rest goals)) cur-solution))))))

(defn print-vars
  [vars bindings]
  (cond
    (fail? bindings) (println "No.")
    (empty? vars) (println "Yes.")
    :else (do
            (doseq [v vars]
              (println v " = " (eval-bindings bindings v)))
            (println))))

(defn print-solutions
  [vars solutions]
  (dorun (map #(print-vars vars %) solutions)))

(defn pretty-prove [goals bindings]
  (print-solutions (variables (clause-body goals)) (prove-all (clause-body goals) bindings)))

(defmacro ?-
  [& goals]
  ; a goal is a rule without a head
  `(pretty-prove '~(vec (concat (list nil) goals)) {}))
