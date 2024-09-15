(ns book-tracker.schemer
  ( :require [clojure.math :as math]))


(defn occur*
  "removes all instances of `kword` from `sexp`, a list of lists of keywords."
  [sexp kword]
  (cond
    (empty? sexp)
    0

    (keyword? (first sexp))
    (cond
      (= (first sexp) kword) (+ 1 (occur* (rest sexp) kword))
      :else (occur* (rest sexp) kword))

    :else
    (+ (occur* (first sexp) kword) (occur* (rest sexp) kword))))


(occur* '(:a :banana (:is :a :banana) (:for (:banana))) :banana)



(defn subst*
  "replaces all instances of `old` with `new` in `l`."
  [new old l]
  (cond
    (empty? l)
    '()

    (keyword? (first l))
    (cond
      (= (first l) old)
      (cons new (subst* new old (rest l)))

      :else
      (cons (first l) (subst* new old (rest l))))

    :else
    (cons (subst* new old (first l)) (subst* new old (rest l)))))

(subst* :orange :banana '(:a :banana (:is :a :banana) (:for (:banana))))



(defn insertL*
  [new old l]
  (cond
    (empty? l)
    '()

    (keyword? (first l))
    (cond
      (= (first l) old)
      (cons new (cons old (insertL* new old (rest l))))

      :else
      (cons (first l) (insertL* new old (rest l))))

    :else
    (cons (insertL* new old (first l)) (insertL* new old (rest l)))))

(insertL*  :orange :banana '(:a :banana (:is :a :banana) (:for (:banana))))


(defn member*
  "returns true if keyword `a` is in `l`, else false."
  [a l]
  (cond
    (empty? l)
    false

    (keyword? (first l))
    (cond
      (= a (first l))
      true

      :else
      (member* a (rest l)))

    :else
    (or (member* a (first l)) (member* a (rest l)))))


(member* :is  '(:a :banana (:is :a :banana) (:for (:banana))))

(member* :not  '(:a :banana (:is :a :banana) (:for (:banana))))

(defn leftmost
  "returns the left-most atom in a non-empty list of sexps that do not contain the empty list"
  [l]
  (cond
    (keyword? (first l))
    (first l)

    :else
    (leftmost (first l))))

(leftmost '((:potato :chips) (:are) :good))


(defn eqlist?
  "checks if two sexps are equal."
  [l1 l2]
  (cond
    (and (seq? l1) (seq? l2))
    (cond
      (and (empty? l1) (empty? l2))
      true

      (or (and (empty? l1) (not-empty l2))
          (and (empty? l2) (not-empty l1)))
      false

      (and (keyword? (first l1)) (keyword? (first l2)))
      (and (= (first l1) (first l2)) (eqlist? (rest l1) (rest l2)))

      (or (and (keyword? (first l1)) (not (keyword? (first l2))))
          (and (keyword? (first l1)) (not (keyword? (first l2)))))
      false

      :else
      (and (eqlist? (first l1) (first l2)) (eqlist? (rest l1) (rest l2))))
    ;; these clauses let it handle just atom comparison too
    ;; technically a different function in the book, called equal? 
    #_(or (and (keyword? l1) (not (keyword? l2)))
        (and (keyword? l2) (not (keyword? l1))))
    #_false

    #_:else
    #_(= l1 l2)))

(eqlist? '((:banana :orange) (:nuts)) '((:banana :orange) (:nuts)))


;; slickified: mutually recursive functions? whoa!

(defn equal? 
  "checks if two sexps OR atoms are equal."
  [s1 s2]
  (cond
    (and (keyword? s1) (keyword? s2))
    (= s1 s2)

    (or (keyword? s1) (keyword? s2))
    false

    :else
    (eqlist? s1 s2)))

(defn eqlist? 
  "checks if two sexps are equal." 
  [l1 l2] 
  (cond 
    (and (empty? l1) (empty? l2))
    true

    (or (empty? l1) (empty? l2))
    false

    :else 
    (and (equal? (first l1) (first l2)) (eqlist? (rest l1) (rest l2)))
    )
  )

(equal? '((:banana :orange) (:nuts)) '((:banana :orange) (:nuts)))
(equal? :banana :orange)
(equal? :banana '(:orange))

(defn numbered?
  [asexp]
  (cond
    (number? asexp)
    true

    :else
    (and (numbered? (first asexp)) (numbered? (first (drop 2 asexp))))))

(numbered?  '(1 + (2 * 3)))

(defn value 
  "compute the value of an arithmetic expression"
  [asexp]
  (cond
    (number? asexp)
    asexp 

    (= (first (rest asexp)) '+)
    (+ (value (first asexp)) (value (first (rest (rest asexp)))))

    (= (first (rest asexp)) '*)
    (* (value (first asexp)) (value (first (rest (rest asexp))))) 

    (= (first (rest asexp)) 'pow)
    (math/pow (value (first asexp)) (value (first (rest (rest asexp)))))
    
    ) 
  )


(value '(1 + (2 * 3)))


;; Chapter 7
(defn member? [lat atom]
  (cond 
    (empty? lat) false
    :else (or (= atom (first lat)) (member? (rest lat) atom))
    )
  )

(member? [1 2 3] 5)


(defn my-set? [lat]
  (cond
    (empty? lat) true
    :else (not (and (member? (rest lat) (first lat)) (my-set? (rest lat))))))


(my-set? '(1 2 3))


(defn make-set [lat]
  (cond
    (empty? lat) lat
    :else (cond
            (member? (rest lat) (first lat)) (make-set (rest lat))
            :else (cons (first lat) (make-set (rest lat))))))

(make-set '(1 1 2 3 4 3)) ;; => (1 2 4 3)


(defn multi-rember [lat member]
  (cond
    (empty? lat) lat
    :else (cond
            (= member (first lat)) (multi-rember (rest lat) member)
            :else (cons (first lat) (multi-rember (rest lat) member)))))

(multi-rember '(1 2 3 1 1) 1)


(defn make-set [lat]
  (cond
    (empty? lat) lat
    :else (cons (first lat) (make-set (multi-rember (rest lat) (first lat))))))


(make-set '(1 1 2 3 4 3)) ;; => (1 2 3 4)










