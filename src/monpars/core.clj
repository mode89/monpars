(ns monpars.core
  (:refer-clojure :exclude [map sequence try]))

; Parser is a function that takes a state and returns the result of the
; parsing along with the remaining state to be parsed.
(defrecord Parser [function])

(defrecord Value
  [value ;; The value returned by the parser.
   state]) ;; The remaining state to be parsed.

(defrecord ParseError
  [message ;; The error message.
   state]) ;; The remaining state to be parsed.

(defmacro make-parser [args & body]
  `(monpars.core/->Parser
     (fn [~@args]
       ~@body)))

(defn run
  "Parses the given string using the given parser and returns the result of
  the parsing."
  [parser state]
  (assert (instance? Parser parser))
  (assert (fn? (:function parser)))
  ((:function parser) state))

(defn return
  "Returns a parser that always succeeds with the given value."
  [x]
  (make-parser [state]
    (->Value x state)))

(defn fail
  "Returns a parser that always fails with the given message."
  [message]
  (make-parser [state]
    (->ParseError message state)))

(defn label
  "Returned parser behaves the same as the given parser, but if the given
  parser fails without consuming any input, the returned parser fails with
  the given message."
  [message parser]
  (make-parser [state]
    (let [result (run parser state)]
      (if (instance? Value result)
        result
        (if (= state (:state result))
          (assoc result :message message)
          result)))))

(defn bind
  "Returns a parser that applies the given function to the value returned
  by the given parser. The function must return a parser. If the given
  parser fails, the returned parser fails."
  [parser f]
  (make-parser [state]
    (let [result (run parser state)]
      (if (instance? Value result)
        (let [parser2 (f (:value result))]
          (run parser2 (:state result)))
        result))))

(defn map
  "When two arguments are given, returns a parser that applies the given
  function to the value returned by the given parser.

  When keyword :to is given as the second argument, returns a parser that
  always returns the given value.
  "
  ([f parser]
   (make-parser [state]
     (let [result (run parser state)]
       (if (instance? Value result)
         (->Value (f (:value result)) (:state result))
         result))))
  ([parser _to value]
   (assert (= _to :to) "The second argument must be :to")
   (monpars.core/map (constantly value) parser)))

(defn satisfy
  "Returns a parser that applies the given parser and then checks if the
  value returned by the parser satisfies the given predicate. If the value
  satisfies the predicate, the returned parser succeeds. Otherwise, the
  returned parser fails."
  [predicate parser]
  (bind parser
    (fn [x]
      (if (predicate x)
        (return x)
        (fail "Does not satisfy predicate")))))

(defn try
  "Returns a parser that behaves like the given parser, except that it
  doesn't consume any input if the given parser fails."
  [parser]
  (make-parser [state]
    (let [result (run parser state)]
      (if (instance? Value result)
        result
        (assoc result :state state)))))

(defn alt
  "This combinator implements choice. It applies the given parsers in order.
  If the first parser succeeds then the result of the first parser is
  returned. If the first parser fails without consuming any input,
  then the second parser is tried."
  [parser1 parser2]
  (make-parser [state]
    (let [result1 (run parser1 state)]
      (if (instance? Value result1)
        result1
        (if (not= state (:state result1))
          ; The first parser consumed input, so we can't try the second
          result1
          ; The first parser didn't consume input, so we can try the second
          (let [result2 (run parser2 state)]
            (if (instance? Value result2)
              ; The second parser succeeded
              result2
              (if (= state (:state result2))
                ; The second parser failed without consuming any input
                (assoc result2 :message
                  (str (:message result1) " or " (:message result2)))
                                                   ; The second parser consumed some input
                result2))))))))

(defn choice
  "Returns a parser that tries each parser in the given sequence. If all
  parsers fail, the returned parser fails."
  [& parsers]
  (reduce alt parsers))

(defn many
  "Returns a parser that applies the given parser zero or more times.
  Returns a vector of the values returned by the given parser.
  If keyword argument :till is given, the parser will stop when
  the given end-parser succeeds."
  ([parser]
   (make-parser [state0]
     (loop [values []
            state state0]
       (let [result (run parser state)]
         (if (instance? Value result)
           (recur (conj values (:value result)) (:state result))
           (if (= state (:state result))
             (->Value values state)
             result))))))
  ([parser _till end-parser]
   (assert (= _till :till) "The second argument must be :till")
   (let [end-marker (Object.)
         end-or-value (alt (map end-parser :to end-marker) parser)]
     (make-parser [state0]
       (loop [values []
              state state0]
         (let [result (run end-or-value state)]
           (if (instance? Value result)
             (if (= (:value result) end-marker)
               (->Value values (:state result))
               (recur (conj values (:value result)) (:state result)))
             result)))))))

(defn skip-many
  "Returns a parser that applies the given parser zero or more times,
  skipping the values returned by the parser."
  [parser]
  (make-parser [state0]
    (loop [state state0]
      (let [result (run parser state)]
        (if (instance? Value result)
          (recur (:state result))
          (->Value nil state))))))

(defn sequence
  "Returns a parser that applies each parser in the given sequence. Returns
  a vector of the values returned by the parsers."
  [parsers]
  (make-parser [state0]
    (loop [remaining-parsers parsers
           values []
           state state0]
      (if (empty? remaining-parsers)
        (->Value values state)
        (let [parser (first remaining-parsers)
              result (run parser state)]
          (if (instance? Value result)
            (recur (rest remaining-parsers)
                   (conj values (:value result))
                   (:state result))
            result))))))

(defmacro let>
  "Binds the given bindings and then evaluates the given body. The bindings
  are evaluated in the order they are given. The body is evaluated in the
  order the bindings are given. Can be used to create a parser that binds
  multiple values in sequence."
  [bindings & body]
  (letfn [(recurse [bs]
            (if (seq bs)
              (let [[bname bvalue & rest-bs] bs
                    inner (recurse rest-bs)]
                `(bind ~bvalue (fn [~bname] ~inner)))
              `(do ~@body)))]
    (recurse bindings)))

(defn not-followed-by
  "Returns a parser that only succeeds if the given parser fails. Does not
  consume any input."
  [parser]
  (make-parser [state]
    (let [result (run parser state)]
      (if (instance? Value result)
        (->ParseError (str "wrong input") state)
        (->Value nil state)))))

(defn predict
  "Returns a parser that parses the given parser without consuming any
  input. If the given parser fails and consumes some input, so does
  the returned parser."
  [parser]
  (make-parser [state]
    (let [result (run parser state)]
      (if (instance? Value result)
        (assoc result :state state)
        result))))

(defn maybe
  "Returns a parser that applies the given parser zero or one times."
  [parser]
  (alt parser (return nil)))
