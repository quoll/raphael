(ns ^{:doc "A namespace for manually parsing Turtle. Entry point is parse-doc."
      :author "Paula Gearon"}
    quoll.raphael
  (:require [clojure.string :as str]))

(def ^:const WIDTH 80)

(defn line-at
  "Returns a line of text that is no more that WIDTH characters. Used for user output."
  [s n]
  (let [line (subs s n (+ n WIDTH))
        nl (str/index-of line \newline)]
    (if nl (subs line 0 nl) line)))

(defn string-builder
  "Creates a mutable string builder.
  init - optional initial string.
  return: an object that can be appended to."
  ([] (StringBuilder.))
  ([init] (StringBuilder. init)))

(defn append!
  "Appends data to a string builders.
  sb - the string builder.
  data - the data to append in string form.
  return: the string builder (may be ignored since it mutates)"
  [sb data]
  (.append sb data))

(defn last-char
  "Returns the last char appended to a string builder.
  Equivalent to `(last (str sb))` but uses direct calls.
  sb - the mutable string builder.
  return: the final char."
  [sb]
  #?(:clj (.charAt sb (dec (.length sb)))
     :cljs (let [s (str sb)] (nth s (dec (count sb))))))

(defn parse-hex
  "Parses a hexadecimal string into a long"
  [s]
  #?(:clj (Long/parseLong s 16)
     :cljs (js/parseInt s 16)))

(defmacro throw-unex
  "Convenience macro to print a message, the offset and the current line.
  msg - the message to print. The line will be appended.
  s - the string that was being parsed.
  n - the offset in the string for the data that caused the error."
  [msg s n]
  `(throw (ex-info (str ~msg (line-at ~s ~n)) {:offset ~n})))

(defprotocol IRI
  (as-iri [iri ns-map] "Returns this object as an iri string. Note that these are typically wrapped in <>"))

(defprotocol NodeGenerator
  (new-node [generator] [generator label]
    "Generate a new node, optionally with a label indicating a reusable node. Return the next generator and node")
  (add-prefix [generator prefix iri]
    "Adds a prefix/iri pair to the namespace map")
  (iri-for [generator prefix]
    "Gets the stored iri for a given prefix"))

(defrecord BlankNode [n]
  Object
  (toString [this] (str "_:b" n)))

(defrecord QName [prefix local]
  IRI
  (as-iri [this generator] (str (iri-for generator prefix) local))
  Object
  (toString [this] (str prefix ":" local)))

(defrecord Generator [counter bnode-cache namespaces]
    NodeGenerator
    (new-node [this]
      [(update this :counter inc) (->BlankNode counter)])
    (new-node [this label]
      (if-let [node (get bnode-cache label)]
        [this node]
        (let [node (->BlankNode counter)]
          [(-> this
               (update :counter inc)
               (update :bnode-cache assoc label node))
           node])))
    (add-prefix [this prefix iri]
      (update this :namespaces assoc prefix iri))
    (iri-for [this prefix]
      (get namespaces prefix)))

(defn new-generator [] (->Generator 0 {} {}))

(defn add-range
  "Adds a range of characters into set.
  chars - the set of characters.
  low - the low end of the character range to add, inclusive.
  high - the high end of the character range to add, inclusive.
  return - the set with the new range of characters added."
  [chars low high]
  (into chars (range (long low) (inc (long high)))))

(def whitespace? #{\space \tab \return \newline})

(def pn-chars-base?
  (-> #{}
      (add-range \A \Z) (add-range \a \z) (add-range \u00C0 \u00D6) (add-range \u00D8 \u00F6)
      (add-range \u00F8 \u02FF) (add-range \u0370 \u037D) (add-range \u037F \u1FFF)
      (add-range \u200C \u200D) (add-range \u2070 \u218F) (add-range \u2C00 \u2FEF)
      (add-range \u3001 \uD7FF) (add-range \uF900 \uFDCF) (add-range \uFDF0 \uFFFD)
      (add-range 0x10000 0xEFFFF)))

(def pn-chars-u? (conj pn-chars-base? \_))

(def pn-chars?
  (-> pn-chars-u?
      (conj \-) (add-range \0 \9) (conj \u00B7)
      (add-range \u0300 \u036F) (add-range \u203F \u2040)))

(def pn-chars-dot? (conj pn-chars? \.))

(def non-iri-char? #{\< \> \" \{ \} \| \^ \`})

(defn dot? [c] (= \. c))
(defn newline? [c] (= \newline c))

(defn skip-whitespace
  "Skip over the whitespace starting from a position in a string.
  s - The string to read.
  n - The starting position.
  return: [n c]
  n - the new offset after skipping whitespace.
  c - the first non-whitespace character, found at position n"
  [s n]
  (loop [n n c (nth s n)]
    (if (whitespace? c)
      (let [n' (inc n)]
        (recur n' (nth s n')))
      [n c])))

(defn skip-to
  "Skip over the whitespace to the required character. If there are nonwhitespace characters, this is an error.
  s - The string to read.
  n - The starting position.
  chars - the set of chars to skip to.
  return: [n c]
  n - the new offset after skipping whitespace to the end of the line.
  c - the first non-whitespace character, found at position n"
  [s n chars]
  (loop [n n c (nth s n)]
    (cond
      (chars c) (let [n' (inc n)]
                  [n' (nth s n')])
      (whitespace? c) (let [n' (inc n)]
                        (recur n' (nth s n')))
      :default (throw-unex "Unexpected characters after end of line: " s n))))

(defn skip-past-dot
  "Skip to the terminating dot. If non-whitespace is found, then report an error.
  s - The string to parse.
  n - The position in the string.
  return: n"
  [s n]
  (let [[n c] (skip-whitespace s n)]
    (if (= \. c)
      (inc n)
      (throw (ex-info (str "Unexpected character found at offset: " n) {:offset n :character c})))))

(defn parse-prefix
  "Parse a prefix. This is a simple string terminated with a ':'. The : character is not part of the prefix.
  s - The string to parse.
  n - The offset to parse from.
  return: [n prefix]
  n - The offset immediately after the prefix.
  prefix - The prefix string."
  [s n]
  (let [sb (string-builder)]
    (loop [n' n c (nth s n)]
      (cond
        (= \: c) (if (= \. (last-char sb))
                   (throw-unex "Unexpected '.' at end of prefix: " s n)
                   [(inc n') (str sb)])
        (or (and (= n n') (pn-chars-base? c))
            (and (not= n n') (pn-chars? c))) (let [n' (inc n')] (append! sb c) (recur n' (nth s n')))
        :default (throw-unex "Unexpected character in prefix: " s n)))))

(defn parse-u-char
  "Parse a code of \\uxxxx or \\Uxxxxxxxx.
  s - the string to parse.
  n - the offset within the string to parse from.
  return: [n char]
  n - the offset immediately after the ucode
  char - the character code that was parsed"
  [s n]
  (let [f (nth s (inc n))
        end (case f
              \u (+ n 6)
              \U (+ n 10)
              nil)]
    (when end
      [end (char (parse-hex (subs s (+ n 2) end)))])))

(defn parse-iri-ref
  "Parse an iri references. This is an iri string surrounded by <> characters. The <> characters are not returned.
  s - The string to parse.
  n - The offset to parse from.
  c - the first character of the iri reference.
  return: [n prefix]
  n - The offset immediately after the prefix.
  prefix - The iri string."
  [s n c]
  (when-not (= c \<)
    (throw-unex "Unexpected character commencing an IRI Reference: " s n))
  (let [sb (string-builder)]
    (loop [n (inc n) c (nth s n)]
      (if (= c \>)
        [(inc n) (str sb)]
        (if (non-iri-char? c)
          (throw-unex "Unexpected character in IRI: " s n)
          (if (= c \\)
            (if-let [[n ch] (parse-u-char s n)]
              (let [n' (inc n)]
                (append! sb ch)
                (recur n' (nth s n')))
              (throw-unex "Unexpected \\ character in IRI: " s n))
            (let [n' (inc n)]
              (append! sb c)
              (recur n' (nth s n')))))))))

(defn parse-prefixed-name
  "Parse a prefix:local-name pair.
  s - The string to parse.
  n - The offset to parse from.
  c - the first character of the prefixed name.
  return: [n prefix]
  n - The offset immediately after the prefixed name.
  qname - The prefixed name as a QName."
  [s n c]
  )

(defn parse-subject
  "Parse a subject entity, including any triples.
  s - the string to parse.
  n - the offset to parse from.
  c - the char found at position n.
  gen - the node generator.
  return: [n subject triples]
  n - the offset immediately after the subject.
  subject - the node for the parsed subject.
  triples - the triples generated in parsing the node."
  [s n c gen]
  ;; TODO: temporarily return an iri ref
  (parse-iri-ref s n c)
  )

(defn parse-object
  "Parse an object entity, including any triples.
  s - the string to parse.
  n - the offset to parse from.
  c - the char found at position n.
  return: [n object triples]
  n - the offset immediately after the object.
  subject - the node for the parsed object.
  triples - the triples generated in parsing the node."
  [s n c gen]
  ;; TODO: temporarily return an iri ref
  (parse-iri-ref s n c)
  )

(defn parse-predicate
  "Parse a predicate entity.
  s - the string to parse.
  n - the offset to parse from.
  c - the char found at position n.
  gen - the generator to use.
  return: [n subject]
  n - the offset immediately after the predicate.
  predicate - the node for the parsed predicate."
  [s n c gen]
  ;; TODO: temporarily return an iri ref
  (parse-iri-ref s n c)
  )

(defn parse-triples
  "Parse a top level triples from a string.
  s - the string to parse from.
  n - the offset in the string to retrieve from.
  c - the character found at position n.
  gen - the generator to use for blank nodes.
  return: [n gen triples]
  n - the new offset after parsing.
  gen - the next generator state.
  triples - the triples generated from parsing this line."
  [s n c gen]
  (let [[n gen subject-triples] (parse-subject s n c gen)
        [n c] (skip-whitespace s n)
        [n gen predicate-triples] (parse-predicate s n c gen)
        [n c] (skip-whitespace s n)
        [n gen object-triples] (parse-object s n c gen)
        n (skip-past-dot s n)]
    [n gen (concat subject-triples predicate-triples object-triples)]))

(defn parse-prefix-iri-end
  "Parse an iri and a newline.
  s - The string to parse from.
  n - The offset to start the parse from.
  gen - The generator to update.
  end-char - A test for the final character
  skip - The number of characters to skip over before looking for the first whitespace
  return: [n gen]
  n - the position of the end of the line.
  gen - the new generator."
  [s n gen end-char skip]
  (if (whitespace? (nth s (+ n skip)))
    (let [[n c] (skip-whitespace s (+ n skip))
          [n prefix] (parse-prefix s n c)
          [n c] (skip-whitespace s n)
          [n iri] (parse-iri-ref s n c)
          n (skip-to s n end-char)]
      [n (add-prefix gen :base iri)])
    (throw-unex "Unknown statement: " s n)))

(defn parse-base-end
  "Parse an iri and a dot.
  s - The string to parse from.
  n - The offset to start the parse from.
  gen - The generator to update.
  end-char - A test for the final character
  skip - The number of characters to skip over before looking for the first whitespace
  return: [n gen]
  n - the position of the end of the line.
  gen - the new generator."
  [s n gen end-char skip]
  (if (whitespace? (nth s (+ n skip)))
    (let [[n c] (skip-whitespace s (+ n skip))
          [n iri] (parse-iri-ref s n c)
          n (skip-to s n end-char)]
      [n (add-prefix gen :base iri)])
    (throw-unex "Unknown statement: " s n)))

(defn parse-statement
  "Parse a directive or triples.
  s - The string to parse.
  n - The position in the string to parse from.
  gen - The current generator.
  return: [n gen triples]
  n - the new offset after parsing.
  gen - the next generator state.
  triples - the triples generated from parsing this line."
  [s n gen]
  (let [[n c] (skip-whitespace s n)
        [n' gen'] (case c
                    \@ (cond
                         (= (str/lower-case (subs s (inc n) (+ n 5))) "base") (parse-base-end s n dot? 5)
                         (= (str/lower-case (subs s (inc n) (+ n 7))) "prefix") (parse-prefix-iri-end s n dot? 7)
                         :default (throw-unex "Unknown statement: " s n))
                    (\B \b) (when (= (str/lower-case (subs s (inc n) (+ n 4)) "ase"))
                              (parse-base-end s n gen newline? 4))
                    (\P \p) (when (= (str/lower-case (subs s (inc n) (+ n 6)) "refix"))
                              (parse-prefix-iri-end s n gen newline? 6))
                    nil)]
    (if n'
      [n' gen' nil]
      (parse-triples s n c gen))))
