(ns com.hypirion.rexf
  (:refer-clojure :exclude [map cat mapcat filter remove take take-while
                            take-nth drop drop-while replace partition-by
                            partition-all keep keep-indexed map-indexed distinct
                            interpose dedupe random-sample
                            ;; reducers (todo: add some more?)
                            conj conj!
                            ;; funcs
                            reduce into])
  (:import (java.util ArrayList)))

(defprotocol ReducerFactory ;; So... it has come to this naming scheme =/
  "A ReducerFactory produces recursive reducers. The reducers may be stateless
  or stateful."
  (init [this] "init returns a reducer"))

(defprotocol Reducer
  "A Reducer is a function with the possibility to reinitialize it. The logic of
  how a reinitialization is done is up to the reducer."
  (reinit [this] "reinit creates a fresh instance of this reducer"))

(defn- verify-reducer-factory
  "Throws an exception if rf is not a ReducerFactory."
  [rf]
  (when-not (extends? ReducerFactory (type rf))
    (throw (ex-info "Value must be reducer factory"
                    {:type (type rf) :val rf}))))

(defprotocol Stateless
  "Being stateless means that x, (init x) and (reinit x) are identical
  and refer to the same object.")

(defn stateless-rf
  "Takes a normal reducer and returns its rexf reducer equivalent. The rexf
  reducer is also its own reducer factory, returning itself."
  [rf]
  (reify clojure.lang.IFn
    (invoke [_] (rf))
    (invoke [_ a] (rf a))
    (invoke [_ a b] (rf a b))
    Reducer
    (reinit [this] this)
    ReducerFactory
    (init [this] this)
    Stateless))

(defn- stateful-rf
  "Takes a normal stateful transducer and a rexf reducer, and returns
  a stateful recursive reducer factory."
  [xf rf]
  (let [rf' (xf rf)]
    (reify clojure.lang.IFn
      (invoke [_] (rf'))
      (invoke [_ a] (rf' a))
      (invoke [_ a b] (rf' a b))
      Reducer
      (reinit [_] (stateful-rf xf (reinit rf))))))

(defn- completing-xf
  "Converts transducer which returns a reducer that only accepts 2-ary calls,
  into a transducer which returns a rexf reducer. When completing or
  init is called, they will be passed down to the reducer function. xf
  is assumed to be stateful."
  [xf rf]
  (let [rf' (xf rf)]
    (reify clojure.lang.IFn
      (invoke [_] (rf))
      (invoke [_ a] (rf a))
      (invoke [_ a b] (rf' a b))
      Reducer
      (reinit [_] (completing-xf xf (reinit rf))))))

(defn- global-stateful-rf
  "Converts a stateful transducer and a rexf reducer into a global stateful rexf
  reducer. A global stateful rexf reducer will pass complete and inits
  downstream unless we are at the topmost recursion level."
  [xf rf]
  (let [rf' (xf rf)]
    (reify clojure.lang.IFn
      (invoke [_] (rf'))
      (invoke [_ a] (rf' a))
      (invoke [_ a b] (rf' a b))
      Reducer
      (reinit [_] (completing-xf xf (reinit rf))))))


(defn stateful-xf
  "Takes a normal stateful transducer and returns a stateful rexf
  transducer."
  [xf]
  (fn [rf]
    (verify-reducer-factory rf)
    (reify ReducerFactory
      (init [this] (stateful-rf xf (init rf))))))

(defn global-xf
  "Takes a function of no arguments which returns a normal stateful transducer,
  and returns a global rexf transducer. The state of the transducer must be
  generated before receiving the reducer function."
  [xf-fn]
  (fn [rf]
    (verify-reducer-factory rf)
    (reify ReducerFactory
      (init [this] (global-stateful-rf (xf-fn) (init rf))))))

(defn toplevel
  "Takes a normal transducer and returns a transducer that only works
  at the topmost reduction level."
  [xf]
  (fn [rfactory]
    (reify ReducerFactory
      (init [this]
        (let [rf (init rfactory)
              rf' (xf rf)]
          (reify clojure.lang.IFn
            (invoke [_] (rf'))
            (invoke [_ a] (rf' a))
            (invoke [_ a b] (rf' a b))
            Reducer
            (reinit [_] (reinit rf))))))))

(defn stateless-xf
  "Takes a stateless transducer and returns a recursive transducer. The
  transducer may or may not be stateless, depending on the reduction function it
  is passed."
  [xf]
  (fn [rf]
    (verify-reducer-factory rf)
    (if (extends? Stateless (type rf)) ;; If stateless, the whole rexf transducer is stateless
      (stateless-rf (xf rf))
      (reify ReducerFactory
        (init [this] (stateful-rf xf (init rf)))))))

(defmacro ^:private defstateless-xf [name]
  `(defn ~name [input#]
     ~(str "Like the transducer version of " name ", but as a stateless rexf transducer.")
     (stateless-xf (~(symbol "clojure.core" (str name)) input#))))
;; (defstateless map) == (defn map [x] (stateless-xf (clojure.core/map x)))

(defmacro ^:private defstateful-xf [name]
  `(defn ~name [input#]
     ~(str "Like the transducer version of " name ", but as a stateful rexf transducer")
     (stateful-xf (~(symbol "clojure.core" (str name)) input#))))
;; (defstateful take) == (defn take [x] (stateful-xf (clojure.core/take x)))

(defmacro ^:private mapmacro [macro names]
  `(do ~@(clojure.core/map list (repeat macro) names)))
;; (mapmacro foo [bar baz quux]) == (do (foo bar) (foo baz) (foo quux))

(mapmacro defstateless-xf [map cat mapcat filter remove keep replace])
(mapmacro defstateful-xf [take take-while take-nth drop drop-while partition-by
                          partition-all keep-indexed map-indexed distinct interpose
                          dedupe random-sample])

(def conj
  "Like conj, but as a rexf reducer."
  (reify clojure.lang.IFn
    (invoke [_] [])
    (invoke [_ coll] coll)
    (invoke [_ coll x] (clojure.core/conj coll x))
    Reducer
    (reinit [this] this)
    ReducerFactory
    (init [this] this)
    Stateless))

(def conj!
  "Like conj!, but as a stateless rexf reducer. Complete converts the
  collection to a persistent version."
  (reify clojure.lang.IFn
    (invoke [_] (transient []))
    (invoke [_ coll] (persistent! coll))
    (invoke [_ coll x]
      (if (nil? coll)
        (prn coll x))
      (.conj ^clojure.lang.ITransientCollection coll x))
    Reducer
    (reinit [this] this)
    ReducerFactory
    (init [this] this)
    Stateless))

(defn conj-from
  "Like conj (in this library). Calling this with 0 arguments is
  equivalent to calling (empty val)."
  [val]
  (let [empty' (empty val)]
    (reify clojure.lang.IFn
      (invoke [_] empty')
      (invoke [_ coll] coll)
      (invoke [_ coll x] (clojure.core/conj coll x))
      Reducer
      (reinit [this] this)
      ReducerFactory
      (init [this] this)
      Stateless)))

(defn conj!-from
  "Like conj! (in this library). Calling this with 0 arguments is
  equivalent to calling (transient (empty val))."
  [val]
  (let [empty' (empty val)]
    (reify clojure.lang.IFn
      (invoke [_] (transient empty'))
      (invoke [_ coll] (persistent! coll))
      (invoke [_ coll x] (.conj ^clojure.lang.ITransientCollection coll x))
      Reducer
      (reinit [this] this)
      ReducerFactory
      (init [this] this)
      Stateless)))

(defn reduce
  "Equivalent to (transduce identity (rexf/init f) init? coll)."
  ([f coll]
   (let [f (init f)]
     (f (clojure.core/reduce f (f) coll))))
  ([f init-val coll]
   (let [f (init f)]
     (f (clojure.core/reduce f init-val coll)))))

(defn into
  "Returns a new coll consisting of to-coll with items conjoined from
  the transducer applied to elements from from-coll. If the reduction
  function is reinitialized, then start emits the result of (empty
  to)."
  [to xf from]
  (if (instance? clojure.lang.IEditableCollection to)
    (reduce (xf (conj!-from to)) (transient to) from)
    (reduce (xf (conj-from to)) to from)))

(defn reduction
  "Returns a reduction of the rexf reducer factory rfac."
  [rfac]
  (let [rf' (init rfac)]
    {:value (rf')
     :rf rf'}))

(defn subreduction
  "Returns a subreduction of the rexf reducer rf."
  [rf]
  (let [rf' (reinit rf)]
    {:value (rf')
     :rf rf'}))

(defn substep
  "Invokes the subreduction with elem."
  [{:keys [value rf] :as v} elem]
  (update v :value rf elem))

(defn subcomplete
  "Completes the subreduction and returns it."
  [{:keys [value rf]}]
  (rf value))

;; Utility functions to group-with*
(defn- stack-last-rf [^ArrayList al rf]
  (if (.isEmpty al)
    rf
    (:rf (.get al (dec (.size al))))))

(defn- stack-last [^ArrayList al]
  (.get al (dec (.size al))))

(defn- stack-reset-last! [^ArrayList al v]
  (.set al (dec (.size al)) v))

(defn- stack-push [^ArrayList al v]
  (.add al v))

(defn- stack-pop [^ArrayList al]
  (.remove al (dec (.size al))))

(defn- stack-add [^ArrayList al v]
  (let [inner (stack-last al)]
    (stack-reset-last! al (substep inner v))
    nil))

(defn- pop-from-stack [^ArrayList al rf res f stop]
  (let [inner (stack-pop al)
        val (subcomplete inner)]
    (if (.isEmpty al)
      (f res rf (:start inner) val stop) ;; attach on res
      (let [inner' (stack-last al)]
        (stack-reset-last! al (update inner' :value f (:rf inner') (:start inner) val stop))
        res)))) ;; is on an inner type: attach on that instead and return res

(defn group-with*
  "group-with* takes two predicates and combiner function, and returns
  a stateful rexf transducer which recursively groups values.

  start-pred must take one argument, the input element, and should return true
  if this starts a new grouping. stop-pred must take two arguments, the first
  the start element, and the second a potential stop element. If this returns
  true, f is called with the arguments res, rf, start-elem, inner values,
  stop-elem.

  Generally you would like to construct a value out of start-elem,
  inner values and stop-elem, then return (rf res constructed), but
  you can produce more values or none at all."
  [start-pred stop-pred f]
  (stateful-xf
   (fn [rf]
     (let [al (ArrayList.)]
       (fn
         ([] (rf))
         ([res] 
          (if (.isEmpty al)
            (rf res)
            (throw (ex-info (str "Unmatched opening value")
                            {:value (:start (stack-last al))
                             :collected (vec al)}))))
         ([res elem]
          (cond (and (not (.isEmpty al))
                     (stop-pred (:start (stack-last al)) elem))
                (pop-from-stack al rf res f elem)
                
                (start-pred elem)
                (do (stack-push al (assoc (subreduction (stack-last-rf al rf))
                                          :start elem))
                    res)
                
                (not (.isEmpty al))
                (do (stack-add al elem)
                    res)
                
                :otherwise (rf res elem))))))))

(defn group-with
  "group-with takes two predicates and combiner function, and returns
  a stateful rexf transducer which recursively groups values.

  start-pred must take one argument, the input element, and should return true
  if this starts a new grouping. stop-pred must take two arguments, the first
  the start element, and the second a potential stop element. If this returns
  true, f is called with the arguments start-elem, inner values and stop-elem,
  and passed to the inner rf."
  [start-pred stop-pred f]
  (group-with* start-pred stop-pred
               (fn [ret rf start inner end]
                 (rf ret (f start inner end)))))
