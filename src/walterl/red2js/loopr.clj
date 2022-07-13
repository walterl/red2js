(ns walterl.red2js.loopr
  "Just the necessary bits from https://github.com/aphyr/dom-top/blob/c6a9fddfebab4bf6e8ddb494eb2e53ea358b5714/src/dom_top/core.clj
  to be able to use the `loopr` macro. See https://aphyr.com/posts/360-loopr-a-loop-reduction-macro-for-clojure
  for a write-up of the macro."
  (:require [riddley.walk :refer [macroexpand-all]])
  (:import [java.lang Iterable]
           [java.util Iterator]))

(deftype Return [value]
  Object
  (toString [this]
    (str "(Return. " (pr-str value) ")")))

(defmethod print-method Return
  [^Return ret ^java.io.Writer w]
  (.write w (.toString ret)))

(defn rewrite-tails*
  "Helper for rewrite-tails which doesn't macroexpand."
  [f form]
  (if-not (seq? form)
    (f form)
    (case (first form)
      (do let* letfn*)
      (list* (concat (butlast form) [(rewrite-tails* f (last form))]))

      if
      (let [[_ test t-branch f-branch] form]
        (list 'if test (rewrite-tails* f t-branch) (rewrite-tails* f f-branch)))

      case*
      (let [[a b c d default clauses & more] form]
        (list* a b c d (rewrite-tails* f default)
               (->> clauses
                    (map (fn [[index [test expr]]]
                           [index [test (rewrite-tails* f expr)]]))
                    (into (sorted-map)))
               more))

      (f form))))

(defn rewrite-tails
  "Takes a Clojure form and invokes f on each of its tail forms--the final
  expression in a do or let, both branches of an if, values of a case, etc."
  [f form]
  (rewrite-tails* f (macroexpand-all form)))

(declare loopr-helper)

(defn loopr-iterator
  "A single loopr layer specialized for traversal using a mutable iterator.
  Builds a form which returns a single accumulator, or a vector of
  accumulators, or a Return, after traversing each x in xs (and more element
  bindings within)."
  [accumulator-bindings [{:keys [lhs rhs] :as eb} & more-element-bindings]
   body {:keys [acc-count] :as opts}]
  (let [accs (map first (partition 2 accumulator-bindings))
        bname (:name eb)
        iter (gensym (str bname "-iter-"))
        res  (gensym (str bname "-res-"))
        rhs  (vary-meta rhs assoc :tag `Iterable)]
    `(let [~iter ^Iterator (.iterator ~rhs)]
       ; Bind each accumulator to itself initially
       (loop [~@(mapcat (juxt identity identity) accs)]
         (if-not (.hasNext ~iter)
           ; We're done iterating
           ~(case (int acc-count)
              0 nil
              1 (first accs)
              `[~@accs])
           (let [~lhs (.next ~iter)]
             ~(if more-element-bindings
                ; More iteration to do within. Descend, come back, recur.
                `(let [~res ~(loopr-helper accumulator-bindings
                                           more-element-bindings
                                           body opts)]
                   (if (instance? Return ~res)
                     ; Early return!
                     ~res
                     (recur ~@(case (int acc-count)
                                0 []
                                1 [res]
                                (map-indexed (fn [i acc] `(nth ~res ~i))
                                             accs)))))
                ; This is the deepest level; use body directly. It'll contain a
                ; compatible recur form. We need to rewrite the body:
                ; (recur x y) -> (recur x y)
                ; x           -> (Return. x)
                (rewrite-tails (fn rewrite-tail [form]
                                 (if (and (seq? form) (= 'recur (first form)))
                                   form
                                   `(Return. ~form)))
                                 body))))))))

(defn loopr-reduce
  "A single loopr layer specialized for traversal using `reduce`. Builds a form
  which returns a single accumulator, or a vector of accumulators, or a Return,
  after traversing each x in xs (and more element bindings within). Reduce is
  often faster over Clojure data structures than an iterator."
  [accumulator-bindings [{:keys [lhs rhs] :as eb} & more-element-bindings]
   body {:keys [acc-count] :as opts}]
  (let [accs (map first (partition 2 accumulator-bindings))
        res  (gensym (str (:name eb) "-res-"))
        acc  (case (int acc-count)
               0 '_
               1 (first accs)
               (vec accs))
        ; The first accumulator we encode in the function arg
        first-acc (case (int acc-count)
                   0 '_
                   (first accs))
        ; Remaining accumulators are stored in volatiles, which we bind to
        ; these expressions each round
        rest-accs (next accs)
        ; The names of each volatile
        rest-acc-volatiles (map-indexed (fn [i acc]
                                          (gensym
                                            (if (symbol? acc)
                                              (str acc "-vol-")
                                              (str i "-vol-"))))
                                        (next accs))
        ; A let binding vector for the initial values of our volatiles
        rest-acc-init-binding (when rest-accs
                                (mapcat (fn [volatile acc]
                                          [volatile `(volatile! ~acc)])
                                        rest-acc-volatiles
                                        rest-accs))
        ; Stores the result of the inner loop
        inner-res  (gensym 'inner-res-)
        ; Stores the result of our reduce
        reduce-res (gensym 'res-)]
    `(let [~@rest-acc-init-binding
           ~reduce-res
       (reduce (fn ~(symbol (str "reduce-" (:name eb))) [~first-acc ~lhs]
                 ; Fetch our current volatiles
                 (let [~@(->> rest-acc-volatiles
                              (map (partial list `deref))
                              (mapcat vector rest-accs))]
                   ~(if more-element-bindings
                      ; More iteration!
                      `(let [~inner-res ~(loopr-helper accumulator-bindings
                                                       more-element-bindings
                                                       body opts)]
                         ; Early return?
                         (if (instance? Return ~inner-res)
                           (reduced ~inner-res)
                           ~(if (< acc-count 2)
                              inner-res
                              ; Update our volatiles and pull out the first acc
                              `(do ~@(map-indexed
                                       (fn [i volatile]
                                         `(vreset! ~volatile
                                                   (nth ~inner-res ~(inc i))))
                                       rest-acc-volatiles)
                                   (first ~inner-res)))))
                      ; This is the deepest level. Rewrite body to replace
                      ; terminal expressions:
                      ; (recur x)   -> x
                      ; (recur x y) -> [x y]
                      ; x           -> (reduced (Return. x))
                      (rewrite-tails
                        (fn rewrite-tail [form]
                          (if (and (seq? form) (= 'recur (first form)))
                            ; Recur
                            (case (int acc-count)
                              0 nil
                              1 (do (assert (= 1 (count (rest form))))
                                    (first (rest form)))
                              ; For multiple accumulators, we want to set the
                              ; rest accs as a side effect, and return the
                              ; first acc.
                              (let [[recur_ first-acc-value & rest-acc-values]
                                    form]
                                `(do ~@(map (fn [volatile value]
                                              `(vreset! ~volatile ~value))
                                            rest-acc-volatiles
                                            rest-acc-values)
                                     ~first-acc-value)))
                            ; Early return
                            `(reduced (Return. ~form))))
                        body))))
               ~(if (zero? acc-count) nil first-acc)
               ~rhs)]
       ~(case (int acc-count)
          ; For 0 or single accs, return the reduce value itself
          (0, 1) reduce-res
          ; With multiple accs, return a vector of their values.
          `(if (instance? Return ~reduce-res)
             ; Early return
             ~reduce-res
             ; Multiple return
             [~reduce-res
              ~@(map (partial list `deref) rest-acc-volatiles)])))))


(defn loopr-array
  "A single loopr layer specialized for traversal over arrays. Builds a form
  which returns a single accumulator, or a vector of accumulators, or a Return,
  after traversing each x in xs using `aget`."
  [accumulator-bindings [{:keys [lhs rhs] :as eb} & more-element-bindings]
   body {:keys [acc-count] :as opts}]
  (let [accs (map first (partition 2 accumulator-bindings))
        bname (:name eb)
        i     (gensym (str bname "-i-"))
        i-max (gensym (str bname "-i-max-"))
        res   (gensym (str bname "-res-"))]
    `(let [~i-max (alength ~rhs)]
       (loop [; Our index into the array
              ~i (int 0)
              ; Initialize each accumulator to itself.
              ~@(mapcat (juxt identity identity) accs)]
         (if (= ~i ~i-max)
           ; Done
           ~(case (int acc-count)
              0 nil
              1 (first accs)
              `[~@accs])
           ; Get an x
           (let [~lhs (aget ~rhs ~i)]
             ~(if more-element-bindings
                ; Descend into inner loop
                `(let [~res ~(loopr-helper accumulator-bindings
                                           more-element-bindings
                                           body opts)]
                   (if (instance? Return ~res)
                     ~res
                     (recur (unchecked-inc-int ~i)
                            ~@(case (int acc-count)
                                0 []
                                1 [res]
                                (map-indexed (fn [i acc] `(nth ~res ~i))
                                             accs)))))
                ; This is the deepest level. Evaluate body, but with early
                ; return for non-recur tails.
                (rewrite-tails (fn rewrite-tail [form]
                                 (if (and (seq? form) (= 'recur (first form)))
                                   `(recur (unchecked-inc-int ~i)
                                           ~@(rest form))
                                   `(Return. ~form)))
                               body))))))))

(defn loopr-helper
  "Helper for building each stage of a nested loopr. Takes an accumulator
  binding vector, a vector of element bindings maps {:lhs, :rhs, :name}, a
  body expression, and an option map with

  :acc-count - The number of accumulators"
  [accumulator-bindings element-bindings body opts]
  (if (empty? element-bindings)
    ; Done!
    body
    ; Generate an iterator loop around the top-level element bindings.
    (let [strategy (case (:via (first element-bindings))
                     :array    loopr-array
                     :iterator loopr-iterator
                     :reduce   loopr-reduce
                     ; With multiple accumulators, vector destructuring can
                     ; make reduce more expensive.
                     nil (if (< 2 (count accumulator-bindings))
                           loopr-iterator
                           ; With single accumulators, Clojure's internal
                           ; reduce is usually more efficient
                           loopr-reduce))]
      (strategy accumulator-bindings element-bindings body opts))))

(defmacro loopr
  "Like `loop`, but for reducing over (possibly nested) collections. Compared to
  `loop`, makes iteration implicit. Compared to reduce, eliminates the need for
  nested reductions, fn wrappers, and destructuring multiple accumulators.
  Compared to `for`, loopr is eager, and lets you carry accumulators.

  Takes an initial binding vector for accumulator variables, (like `loop`);
  then a binding vector of loop variables to collections (like `for`); then a
  body form, then an optional final form. Iterates over each element of the
  collections, like `for` would, and evaluates body with that combination of
  elements bound.

  Like `loop`, the body should generally contain one or more (recur ...) forms
  with new values for each accumulator. Any non-recur form in tail position
  causes loopr to return that value immediately.

  When the loop completes normally, loopr returns:

  - The value of the final expression, which has access to the accumulators, or
  - If no `final` is given...
    - With zero accumulators, returns `nil`
    - With one accumulator, returns that accumulator
    - With multiple accumulators, returns a vector of each.

  For example,

    (loopr [sum 0]
           [x [1 2 3]]
      (recur (+ sum x)))

  returns 6: the sum of 1, 2 and 3.

  This would typically be written as `(reduce + [1 2 3])`, and for single
  accumulators or single loops using `reduce` or `loop` is often more concise.
  Loopred's power comes from its ability to carry multiple accumulators and to
  traverse multiple dimensions. For instance, to get the mean of all elements
  in a matrix:

    (loopr [count 0
            sum   0]
           [row [[1 2 3] [4 5 6] [7 8 9]]
            x   row]
      (recur (inc count) (+ sum x))
      (/ sum count))
    ; returns 45/9 = 5

  Here, we have a body which recurs, and a final expression `(/ sum count)`,
  which is evaluated with the final value of the accumulators. Compare this to
  the equivalent nested reduce:

    (let [[sum count] (reduce (fn [[count sum] row]
                                (reduce (fn [[count sum] x]
                                          [(inc count) (+ sum x)])
                                        [count sum]
                                        row))
                              [0 0]
                              [[1 2 3] [4 5 6] [7 8 9]])]
      (/ sum count))

  This requires an enclosing `let` binding to transform the loop results, two
  calls to reduce, each with their own function, creating and destructuring
  vectors at each level, and keeping track of accumulator initial values far
  from their point of use. The structure of accumulators is encoded in five
  places instead of two, which makes it harder to change accumulators later.
  It also requires deeper indentation. Here's the same loop expressed as a
  flat `loop` over seqs:

    (loop [count 0
           sum   0
           rows  [[1 2 3] [4 5 6] [7 8 9]]
           row   (first rows)]
      (if-not (seq rows)
        (/ sum count)       ; Done with iteration
        (if-not (seq row)   ; Done with row; move on to next row
          (recur count sum (next rows) (first (next rows)))
          (let [[x & row'] row]
            (recur (inc count) (+ sum x) rows row')))))

  This version is less indented but also considerably longer, and the
  interweaving of traversal machinery and accumulation logic makes it
  difficult to understand. It is also significantly slower than the nested
  `reduce`, on account of seq allocation--vectors can more efficiently reduce
  over their internal structure.

  Depending on how many accumulators are at play, and which data structures are
  being traversed, it may be faster to use `loop` with an iterator, `loop` with
  `aget`, or `reduce` with a function. loopr compiles to (possibly nested)
  `reduce` when given a single accumulator, and to (possibly nested) `loop`
  with mutable iterators when given multiple accumulators. You can also control
  the iteration tactic for each collection explicitly:

    (loopr [count 0
            sum   0]
           [row [[1 2 3] [4 5 6] [7 8 9]] :via :reduce
            x   row                       :via :iterator]
      (recur (inc count) (+ sum x))
      (/ sum count))

  This compiles into a `reduce` over rows, and a `loop` over each row using an
  iterators. For array iteration, use `:via :array`:

    (loopr [sum 0]
           [x (long-array (range 10000)) :via :array]
           (recur (+ sum x)))
    ; => 49995000

  Note that alength/aget are *very* sensitive to type hints; use `lein check`
  to ensure that you're not using reflection, and add type hints as necessary.
  On my older xeon, this is roughly an order of magnitude faster than (reduce +
  longs). For nested array reduction, make sure to hint inner collections, like
  so:

    (loopr [sum 0]
           [row                        matrix :via :array
            x   ^\"[Ljava.lang.Long;\" row    :via :array]
           (recur (+ sum x)))))

  Like `loop`, `loopr` supports early return. Any non `(recur ...)` form in
  tail position in the body is returned immediately, without visiting any other
  elements in the collection(s). To search for the first odd number in
  collection, returning that number and its index:

    (loopr [i 0]
           [x [0 3 4 5]]
           (if (odd? x)
             {:i i, :x x}
             (recur (inc i))))
    ; => {:i 1, :x 3}

  When no accumulators are provided, loopr still iterates, returning any
  early-returned value, or the final expression when iteration completes, or
  `nil` otherwise. Here we find an key in a map by value. Note that we can also
  destructure in iterator bindings.

    (loopr []
           [[k v] {:x 1, :y 2}]
           (if (= v 2)
             k
             (recur))
           :not-found)
    ; => :y"
  [accumulator-bindings element-bindings body & [final]]
  (assert (<= 2 (count element-bindings))) ; TODO: determine semantics for this?
  (assert (even? (count accumulator-bindings)))
  (assert (even? (count element-bindings)))
  ; Parse element bindings into a vector of maps
  (let [element-bindings
        (loop [forms     element-bindings
               bindings  []]
          (if-not (seq forms)
            bindings
            (let [[f1 f2 & fs] forms
                  i (count bindings)]
              (if (keyword? f1)
                ; Options for last binding
                (case f1
                  :via (recur fs (update bindings (dec i) assoc :via f2))
                  (throw (IllegalArgumentException.
                           (str "Unrecognized element binding option: "
                                (pr-str f1)
                                " - expected :via"))))
                ; New binding
                (let [; Choose a friendly name for this binding.
                      binding-name (if (symbol? f1)
                                     f1
                                     (symbol (str "iter-" i)))
                      binding {:name binding-name
                               :lhs  f1
                               :rhs  f2}]
                  (recur fs (conj bindings binding)))))))
        acc-names   (map first (partition 2 accumulator-bindings))
        acc-count   (count acc-names)
        acc         (case acc-count
                      0 (gensym 'res-)
                      1 (first acc-names)
                      (vec acc-names))
        opts        {:acc-count acc-count}
        res         (gensym 'res-)]
    `(let [~@accumulator-bindings
           ~res ~(loopr-helper accumulator-bindings element-bindings body opts)]
       (if (instance? Return ~res)
         (.value ~(vary-meta res assoc :tag `Return))
         ~(if final
            `(let [~acc ~res] ~final)
            res)))))
