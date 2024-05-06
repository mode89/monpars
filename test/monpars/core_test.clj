(ns monpars.core-test
  (:refer-clojure :exclude [sequence])
  (:require [clojure.test :refer [deftest is]]
            [monpars.core :as pa]))

(def any-element
  (pa/make-parser [state]
    (if (empty? state)
      (pa/->ParseError "any-element" state)
      (pa/->Value (first state) (rest state)))))

(defn element [e]
  (pa/make-parser [state]
    (if (= e (first state))
      (pa/->Value e (rest state))
      (pa/->ParseError (str "element: " e) state))))

(defn pair [e1 e2]
  (pa/make-parser [state]
    (if (= e1 (first state))
      (if (= e2 (second state))
        (pa/->Value [e1 e2] (drop 2 state))
        (pa/->ParseError (str "element: " e2) (drop 1 state)))
      (pa/->ParseError (str "element: " e1) state))))

(defn success [value state]
  (pa/->Value value state))

(defn failure [value state]
  (pa/->ParseError value state))

(deftest return
  (let [p (partial pa/run (pa/return 42))]
    (is (= (p '()) (success 42 '())))
    (is (= (p '(1)) (success 42 '(1))))))

(deftest fail
  (let [p (partial pa/run (pa/fail "nope"))]
    (is (= (p '()) (failure "nope" '())))
    (is (= (p '(1)) (failure "nope" '(1))))))

(deftest label
  (let [p (partial pa/run (pa/label "foo" (pair 1 2)))]
    (is (= (p '(1 2)) (success '(1 2) '())))
    (is (= (p '(2 2)) (failure "foo" '(2 2))))
    (is (= (p '(1 3)) (failure "element: 2" '(3))))))

(deftest bind
  (let [p (partial pa/run
            (pa/bind any-element
              (fn [x]
                (pa/bind any-element
                  (fn [y]
                    (pa/return (+ x y)))))))]
    (is (= (p '()) (failure "any-element" '())))
    (is (= (p '(1)) (failure "any-element" '())))
    (is (= (p '(1 2)) (success 3 '())))
    (is (= (p '(1 2 3)) (success 3 '(3))))
    (is (= (p '(4 5 6 7)) (success 9 '(6 7))))))

(deftest map-parser
  (let [p (partial pa/run (pa/map inc any-element))]
    (is (= (p '()) (failure "any-element" '())))
    (is (= (p '(1)) (success 2 '())))
    (is (= (p '(2)) (success 3 '())))
    (is (= (p '(42)) (success 43 '())))
    (is (= (p '(4 42)) (success 5 '(42)))))
  (let [p (partial pa/run (pa/map any-element :to 42))]
    (is (= (p '()) (failure "any-element" '())))
    (is (= (p '(1)) (success 42 '()))))
  (is (thrown-with-msg? AssertionError #"second argument must be :to"
        (pa/map any-element :too 42))))

(deftest satisfy
  (let [p (partial pa/run (pa/satisfy odd? any-element))]
    (is (= (p '(1)) (success 1 '())))
    (is (= (p '(2)) (failure "Does not satisfy predicate" '())))))

(deftest try
  (let [p (partial pa/run (pa/try (pair 1 2)))]
    (is (= (p '(1 2)) (success [1 2] '())))
    (is (= (p '(1 3)) (failure "element: 2" '(1 3))))))

(deftest alt
  (let [p (partial pa/run (pa/alt (element 1) (element 2)))]
    (is (= (p '(1)) (success 1 '())))
    (is (= (p '(2)) (success 2 '())))
    (is (= (p '(3)) (failure "element: 1 or element: 2" '(3)))))
  (let [p (partial pa/run (pa/alt (pair 1 2) (pair 3 4)))]
    (is (= (p '(1 3)) (failure "element: 2" '(3))))
    (is (= (p '(3 5)) (failure "element: 4" '(5))))))

(deftest choice
  (let [p (partial pa/run
            (pa/choice (pair 1 2) (pair 3 4) (pair 5 6)))]
    (is (= (p '(1 2)) (success [1 2] '())))
    (is (= (p '(3 4)) (success [3 4] '())))
    (is (= (p '(5 6)) (success [5 6] '())))
    (is (= (p '(7)) (failure "element: 1 or element: 3 or element: 5" '(7))))
    (is (= (p '(3 5)) (failure "element: 4" '(5))))))

(deftest many
  (let [p (partial pa/run (pa/many (element 1)))]
    (is (= (p '()) (success [] '())))
    (is (= (p '(2)) (success [] '(2))))
    (is (= (p '(1)) (success [1] '())))
    (is (= (p '(1 1)) (success [1 1] '())))
    (is (= (p '(1 1 2)) (success [1 1] '(2)))))
  (let [p (partial pa/run (pa/many (pair 1 2)))]
    (is (= (p '(1 2 1 2)) (success [[1 2] [1 2]] '())))
    (is (= (p '(1 2 1 2 3)) (success [[1 2] [1 2]] '(3))))
    (is (= (p '(1 2 1 2 1)) (failure "element: 2" '()))))
  (is (thrown-with-msg? AssertionError #"second argument must be :till"
        (pa/many (element 1) :til (element 42))))
  (let [p (partial pa/run (pa/many (element 1) :till (element 42)))]
    (is (= (p '()) (failure "element: 42 or element: 1" '())))
    (is (= (p '(2)) (failure "element: 42 or element: 1" '(2))))
    (is (= (p '(42)) (success [] '())))
    (is (= (p '(1 42)) (success [1] '())))
    (is (= (p '(1 1 42)) (success [1 1] '())))
    (is (= (p '(1 1 2 42)) (failure "element: 42 or element: 1" '(2 42)))))
  (let [p (partial pa/run (pa/many (pair 1 2) :till (pair 3 4)))]
    (is (= (p '(1 2 3 4)) (success [[1 2]] '())))
    (is (= (p '(1 2 1 3 4)) (failure "element: 2" '(3 4))))
    (is (= (p '(1 2 1 2 3 5 4)) (failure "element: 4" '(5 4))))))

(deftest skip-many
  (let [p (partial pa/run (pa/skip-many (element 1)))]
    (is (= (p '()) (success nil '())))
    (is (= (p '(2)) (success nil '(2))))
    (is (= (p '(1)) (success nil '())))
    (is (= (p '(1 1)) (success nil '())))
    (is (= (p '(1 1 2)) (success nil '(2))))))

(deftest sequence
  (let [p (partial pa/run
            (pa/sequence [(element 1) (element 2)]))]
    (is (= (p '()) (failure "element: 1" '())))
    (is (= (p '(1)) (failure "element: 2" '())))
    (is (= (p '(1 2)) (success [1 2] '())))
    (is (= (p '(1 2 3)) (success [1 2] '(3))))))

(deftest let>
  (let [p (partial pa/run
            (pa/let> [a (element 1)
                      b (element 2)
                      c any-element]
              (pa/return (+ a b c))))]
    (is (= (p '()) (failure "element: 1" '())))
    (is (= (p '(1)) (failure "element: 2" '())))
    (is (= (p '(1 2)) (failure "any-element" '())))
    (is (= (p '(1 2 3)) (success 6 '())))
    (is (= (p '(1 2 4 5)) (success 7 '(5))))))

(deftest not-followed-by
  (let [p (partial pa/run (pa/not-followed-by (element 1)))]
    (is (= (p '()) (success nil '())))
    (is (= (p '(2)) (success nil '(2))))
    (is (= (p '(1)) (failure "wrong input" '(1))))))

(deftest predict
  (let [p (partial pa/run (pa/predict (element 1)))]
    (is (= (p '()) (failure "element: 1" '())))
    (is (= (p '(2)) (failure "element: 1" '(2))))
    (is (= (p '(1)) (success 1 '(1))))))

(deftest maybe
  (let [p (partial pa/run (pa/maybe (pair 1 2)))]
    (is (= (p '(1 2)) (success '(1 2) '())))
    (is (= (p '(2 3)) (success nil '(2 3))))
    (is (= (p '(1 3)) (failure "element: 2" '(3))))))
