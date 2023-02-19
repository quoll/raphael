(ns ^{:doc "Objects for moving triples"
      :author "Paula Gearon"}
    quoll.raphael.triples
  (:refer-clojure :exclude [seq])
  #?(:clj (:import [clojure.lang Seqable Counted])))

(defprotocol TripleSink
  (append! [sink subject predicate object] "Statefully appends a triple"))

;; A stateful accumulator of Triples
(deftype TripleAccumulator [vtriples]
  TripleSink
  (append!
    [this s p o]
    (vswap! vtriples conj! [s p o])
    this)

  #?(:clj Seqable :cljs ISeqable)
  (#?(:clj seq :cljs -seq) [_] (clojure.core/seq (persistent! @vtriples)))

  #?(:clj Counted :cljs ICounted)
  (#?(:clj count :cljs -count) [_] (count @vtriples)))

(defn triple-accumulator
  "Creates a new triple accumulator"
  []
  (->TripleAccumulator (volatile! (transient []))))
