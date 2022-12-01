(ns ^{:doc "A namespace for manually parsing Turtle. Entry point is parse-doc."
      :author "Paula Gearon"}
    quoll.raphael.core
  (:require [clojure.string :as str]
            [quoll.raphael.text :as text :refer [char-at]]))

(def ^:const WIDTH 80)

(defn line-at
  "Returns a line of text that is no more that WIDTH characters. Used for user output."
  [s n]
  (let [line (subs s n (min (+ n WIDTH) (count s)))
        nl (str/index-of line \newline)]
    (if nl (subs line 0 nl) line)))

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

(defrecord QName [prefix local iri]
  IRI
  (as-iri [this generator] (str (iri-for generator prefix) local))
  Object
  (toString [this] (str prefix ":" local)))

(defn qname
  "Creates a qname"
  ([prefix local] (->QName prefix local nil))
  ([prefix local gen] (->QName prefix local (str (iri-for gen prefix) local))))

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
  (into chars (map char) (range (long low) (inc (long high)))))

(def whitespace? #{\space \tab \return \newline})

(def hex? (-> #{} (add-range \0 \9) (add-range \A \F) (add-range \a \f)))

(def pn-chars-base?
  (-> #{}
      (add-range \A \Z) (add-range \a \z) (add-range \u00C0 \u00D6) (add-range \u00D8 \u00F6)
      (add-range \u00F8 \u02FF) (add-range \u0370 \u037D) (add-range \u037F \u1FFF)
      (add-range \u200C \u200D) (add-range \u2070 \u218F) (add-range \u2C00 \u2FEF)
      (add-range \u3001 \uD7FF) (add-range \uF900 \uFDCF) (add-range \uFDF0 \uFFFD)))

;; (range 0x10000 0xEFFFF) will be taken care of by the high/low surrogate tests

(defn high-surrogate?
  "Tests if a character is both a high surrogate (0xD800 <= c <= 0xDBFF)
  and also in range (0xD800 <= c <= 0xDB7F) which matches the character range 0x10000 to 0xEFFFF"
  [c]
  (<= 0xD800 (long c) 0xDB7F))

(defn low-surrogate?
  "Tests if a character is a low surrogate (0xDC00 <= c <= 0xDFFF)
  which matches the character range 0x10000 to 0xEFFFF"
  [c]
  (<= 0xDC00 (long c) 0xDFFF))

(def pn-chars-u? (conj pn-chars-base? \_))

(def pn-chars?
  (-> pn-chars-u?
      (conj \-)
      (add-range \0 \9)
      (conj \u00B7)
      (add-range \u0300 \u036F)
      (add-range \u203F \u2040)))

(def pn-chars-dot? (conj pn-chars? \.))

(def local-chars? (-> pn-chars-u?
                      (add-range \0 \9)
                      (conj \:)
                      (conj \%)
                      (conj \\)))

(def local-chars2? (-> pn-chars?
                       (conj \:)
                       (conj \.)
                       (conj \%)
                       (conj \\)))

(def local-esc? #{\_ \~ \. \- \! \$ \& \' \( \) \* \+ \, \; \= \/ \? \# \@ \%})

(def non-iri-char? #{\< \> \" \{ \} \| \^ \` \space :eof})

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
  (loop [n n c (char-at s n)]
    (if (= :eof c)
      [-1 :eof]
      (if (whitespace? c)
        (let [n' (inc n)]
          (recur n' (char-at s n')))
        [n c]))))

(defn skip-to
  "Skip over the whitespace to the required character. If there are nonwhitespace characters, this is an error.
  s - The string to read.
  n - The starting position.
  chars - the set of chars to skip to.
  return: [n c]
  n - the new offset after skipping whitespace to the end of the line.
  c - the first non-whitespace character, found at position n"
  [s n chars]
  (loop [n n c (char-at s n)]
    (cond
      (chars c) (let [n' (inc n)]
                  [n' (char-at s n')])
      (whitespace? c) (let [n' (inc n)]
                        (recur n' (char-at s n')))
      :default (throw-unex "Unexpected characters after end of line: " s n))))

(defn skip-past-dot
  "Skip to the terminating dot. If non-whitespace is found, then report an error.
  s - The string to parse.
  n - The position in the string.
  return: [n c]"
  [s n]
  (let [[n c] (skip-whitespace s n)]
    (if (= \. c)
      (let [n' (inc n)]
        [n' (char-at s n')])
      (throw (ex-info (str "Unexpected character found at offset: " n) {:offset n :character c})))))

(defn parse-prefix
  "Parse a prefix. This is a simple string terminated with a ':'. The : character is not part of the prefix.
  s - The string to parse.
  n - The offset to parse from.
  c - The character at offset n.
  return: [n c prefix]
  n - The offset immediately after the prefix.
  c - The character at offset n.
  prefix - The prefix string."
  [s n c]
  (let [sb (text/string-builder)]
    (loop [n' n c (char-at s n)]
      (cond
        (= \: c) (if (= \. (text/last-char sb))
                   (throw-unex "Unexpected '.' at end of prefix: " s n)
                   (let [nn' (inc n')]
                     [nn' (char-at s nn') (str sb)]))
        (= n n') (cond
                   (pn-chars-base? c) (let [n' (inc n')]
                                        (text/append! sb c)
                                        (recur n' (char-at s n')))
                   (high-surrogate? c) (let [nn (+ n' 2)
                                             c2 (char-at s (inc n'))]
                                         (when-not (low-surrogate? c2)
                                           (throw-unex "Bad Unicode characters at start of prefix: " s n))
                                         (text/append! sb c)
                                         (text/append! sb c2)
                                         (recur nn (char-at s nn)))
                   :default (throw-unex "Unexpected character at start of prefix: " s n))
        (pn-chars? c) (let [n' (inc n')]
                        (text/append! sb c)
                        (recur n' (char-at s n')))
        (high-surrogate? c) (let [nn (+ n' 2)
                                  c2 (char-at s (inc n'))]
                              (when-not (low-surrogate? c2)
                                (throw-unex "Bad Unicode characters in prefix: " s n))
                              (text/append! sb c)
                              (text/append! sb c2)
                              (recur nn (char-at s nn)))
        :default (throw-unex (str "Unexpected character '" c "' in prefix: ") s n)))))

(defn parse-u-char
  "Parse a code of \\uxxxx or \\Uxxxxxxxx.
  s - the string to parse.
  n - the offset within the string to parse from.
  return: [n char]
  n - the offset immediately after the ucode
  char - the character code that was parsed"
  [s n]
  (let [f (char-at s (inc n))
        end (case f
              \u (+ n 6)
              \U (+ n 10)
              nil)]
    (when end
      [end (char (text/parse-hex (subs s (+ n 2) end)))])))

;; A regex to find the scheme at the start of an IRI
(def scheme-re #"^[A-Za-z][A-Za-z0-9.+-]*:")

(defn relative-iri?
  "Indicates if an IRI is relative."
  [s]
  (nil? (re-find scheme-re s)))

(defn parse-iri-ref
  "Parse an iri references. This is an iri string surrounded by <> characters. The <> characters are not returned.
  s - The string to parse.
  n - The offset to parse from.
  c - the first character of the iri reference.
  gen - the current generator
  return: [n iri]
  n - The offset immediately after the prefix.
  iri - The iri string."
  [s n c gen]
  (when-not (= c \<)
    (throw-unex "Unexpected character commencing an IRI Reference: " s n))
  (let [sb (text/string-builder)]
    (loop [n (inc n) c (char-at s n)]
      (if (= c \>)
        (let [i (str sb)
              iri (if (and gen (relative-iri? i))
                    (if-let [base (iri-for gen :base)] (str base i) i)
                    i)
              n' (inc n)]
          [n' (char-at s n') iri])
        (if (non-iri-char? c)
          (throw-unex "Unexpected character in IRI: " s n)
          (if (= c \\)
            (if-let [[n ch] (parse-u-char s n)]
              (let [n' (inc n)]
                (text/append! sb ch)
                (recur n' (char-at s n')))
              (throw-unex "Unexpected \\ character in IRI: " s n))
            (let [n' (inc n)]
              (text/append! sb c)
              (recur n' (char-at s n')))))))))

(defn parse-local
  "Parse a local into a string.
  s - The string to parse.
  n - The offset to parse from.
  return: [n c local]
  n - the offset immediately after the local name.
  c - the character at offset n.
  local - the parsed local name."
  [s n]
  (let [sb (text/string-builder)
        add-char (fn [c n]  ;; adds a char, looking ahead if this is an escape sequence
                   (text/append! sb c)
                   (case c
                     \% (let [a (char-at s (inc n))
                              b (char-at s (+ n 2))]
                          (if (and (hex? a) (hex? b))
                            (do
                              (text/append! sb a)
                              (text/append! sb b)
                              (+ n 3))
                            (throw-unex "Bad escape code in localname: " s n)))
                     \\ (let [a (char-at s (inc n))]
                          (if (local-esc? a)
                            (do
                              (text/append! sb a)
                              (+ n 2))
                            (throw-unex "Bad escape code in localname: " s n)))
                     (inc n)))
        f (char-at s n)
        _ (when-not (local-chars? f) (throw-unex (str "Unexpected character '" f "' in local name: ") s n))
        n (add-char f n)]
    (loop [n n c (char-at s n)]
      (if (= \. c) ;; at a dot. Check if this is inside a local name or terminating it
        (let [n' (inc n) ;; look ahead
              c' (char-at s n')]
          (if (local-chars2? c')
            ;; the next character is a valid local name character, so save and continue parsing
            (do
              (text/append! sb c) ;; a dot, so don't need to check for PLX (%hh or \ escape)
              (recur n' c'))
            ;; no, this must mean the local name ended already, and the dot is terminating a line
            ;; return the current name, along with the position of the dot
            [n c (str sb)]))
        (if (local-chars2? c)
          ;; a valid local name char, so save and continue
          (let [n' (add-char c n)]
            (recur n' (char-at s n')))
          [n c (str sb)])))))

(defn parse-prefixed-name
  "Parse a prefix:local-name pair.
  s - The string to parse.
  n - The offset to parse from.
  c - the first character of the prefixed name.
  gen - the generator
  return: [n c prefix]
  n - The offset immediately after the prefixed name.
  c - The character immediately after the prefixed name.
  qname - The prefixed name as a QName."
  [s n c gen]
  (when-not (or (pn-chars-base? c) (= \: c))
    (throw-unex "Prefix char starts with illegal character" s n))
  (let [sb (text/string-builder)
        [n prefix] (loop [n n c c dot false]
                     (if (= \: c)
                       (if dot
                         (throw-unex "Prefix illegally ends with a '.': " s n)
                         [(inc n) (str sb)])
                       (if (pn-chars-dot? c)
                         (let [n' (inc n)]
                           (text/append! sb c)
                           (recur n' (char-at s n') (dot? c)))
                         (throw-unex (str "Illegal character '" c "' in prefix: ") s n))))
        [n c local] (parse-local s n)]
    [n c (qname prefix local gen)]))

(defn parse-collection
  [s n c gen]
  )

(defn parse-blank-node
  [s n c gen]
  )

(defn parse-blank-node-entity
  [s n c gen]
  )

(defn parse-literal
  [s n c gen]
  )

(defn parse-number
  [s n c]
  )

(defn parse-subject
  "Parse a subject entity, including any triples.
  s - the string to parse.
  n - the offset to parse from.
  c - the char found at position n.
  gen - the node generator.
  return: [n c subject triples]
  n - the offset immediately after the subject.
  c - the character at offset n.
  subject - the node for the parsed subject.
  triples - the triples generated in parsing the node."
  [s n c gen]
  (case c
    \< (parse-iri-ref s n c gen)
    \( (parse-collection s n c gen)
    \_ (parse-blank-node s n c)
    \[ (parse-blank-node-entity s n c gen)
    (parse-prefixed-name s n c gen)))

(defn parse-object
  "Parse an object entity, including any triples.
  s - the string to parse.
  n - the offset to parse from.
  c - the char found at position n.
  return: [n c object triples]
  n - the offset immediately after the object.
  c - the character at offset n.
  subject - the node for the parsed object.
  triples - the triples generated in parsing the node."
  [s n c gen]
  (case c
    \< (parse-iri-ref s n c gen)
    \( (parse-collection s n c gen)
    \_ (parse-blank-node s n c)
    \[ (parse-blank-node-entity s n c gen)
    (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \. \+ \-) (parse-number s n c)
    (\" \') (parse-literal s n c gen)
    (cond
      (and (= c \f)
           (= "alse" (text/ssubs s n (+ n 5)))
           (not (pn-chars? (char-at s (+ n 5))))) (let [n' (+ n 5)]
                                                    [n' (char-at s n') false])
      (and (= c \t)
           (= "rue" (text/ssubs s n (+ n 4)))
           (not (pn-chars? (char-at s (+ n 4))))) (let [n' (+ n 4)]
                                                    [n' (char-at s n') true])
      :default (parse-prefixed-name s n c gen))))

(defn parse-predicate
  "Parse a predicate entity.
  s - the string to parse.
  n - the offset to parse from.
  c - the char found at position n.
  gen - the generator to use.
  return: [n c subject]
  n - the offset immediately after the predicate.
  c - the character at offset n.
  predicate - the node for the parsed predicate."
  [s n c gen]
  (if (= \< c)
    (parse-iri-ref s n c gen)
    (parse-prefixed-name s n c gen)))

(defn parse-triples
  "Parse a top level triples from a string.
  s - the string to parse from.
  n - the offset in the string to retrieve from.
  c - the character found at position n.
  gen - the generator to use for blank nodes.
  return: [n c gen triples]
  n - the new offset after parsing.
  c - the character at offset n.
  gen - the next generator state.
  triples - the triples generated from parsing this line."
  [s n c gen]
  (let [[n c gen subject-triples] (parse-subject s n c gen)
        [n c] (skip-whitespace s n)
        [n c gen predicate-triples] (parse-predicate s n c gen)
        [n c] (skip-whitespace s n)
        [n c gen object-triples] (parse-object s n c gen)
        [n c] (skip-past-dot s n)]
    [n c gen (concat subject-triples predicate-triples object-triples)]))

(defn parse-prefix-iri-end
  "Parse an iri and a newline.
  s - The string to parse from.
  n - The offset to start the parse from.
  gen - The generator to update.
  end-char - A test for the final character
  skip - The number of characters to skip over before looking for the first whitespace
  return: [n c gen]
  n - the position of the end of the line.
  c - character at position n
  gen - the new generator."
  [s n gen end-char skip]
  (if (whitespace? (char-at s (+ n skip)))
    (let [[n c] (skip-whitespace s (+ n skip))
          [n c prefix] (parse-prefix s n c)
          [n c] (skip-whitespace s n)
          [n c iri] (parse-iri-ref s n c gen)
          [n c] (skip-to s n end-char)]
      [n c (add-prefix gen prefix iri)])
    (throw-unex "Unknown statement: " s n)))

(defn parse-base-end
  "Parse an iri and a dot.
  s - The string to parse from.
  n - The offset to start the parse from.
  gen - The generator to update.
  end-char - A test for the final character
  skip - The number of characters to skip over before looking for the first whitespace
  return: [n c gen]
  n - the position of the end of the line.
  c - character at position n
  gen - the new generator."
  [s n gen end-char skip]
  (if (whitespace? (char-at s (+ n skip)))
    (let [[n c] (skip-whitespace s (+ n skip))
          [n c iri] (parse-iri-ref s n c nil)  ;; use a nil generator, since an existing base should not be used
          [n c] (skip-to s n end-char)]
      [n c (add-prefix gen :base iri)])
    (throw-unex "Unknown statement: " s n)))

(defn parse-statement
  "Parse a directive or triples.
  s - The string to parse.
  n - The position in the string to parse from.
  gen - The current generator.
  return: [n c gen triples]
  n - the new offset after parsing.
  c - the character at position n.
  gen - the next generator state.
  triples - the triples generated from parsing this line."
  [s n gen]
  (let [[n c] (skip-whitespace s n)
        [n' c' gen'] (case c
                       \@ (cond
                            (= (str/lower-case (subs s (inc n) (+ n 5))) "base") (parse-base-end s n gen dot? 5)
                            (= (str/lower-case (subs s (inc n) (+ n 7))) "prefix") (parse-prefix-iri-end s n gen dot? 7)
                            :default (throw-unex "Unknown statement: " s n))
                       (\B \b) (when (and (= (str/lower-case (subs s (inc n) (+ n 4))) "ase")
                                          (whitespace? (char-at s (+ n 4))))
                                 (parse-base-end s n gen newline? 4))
                       (\P \p) (when (and (= (str/lower-case (subs s (inc n) (+ n 6))) "refix")
                                          (whitespace? (char-at s (+ n 6))))
                                 (parse-prefix-iri-end s n gen newline? 6))
                       nil)]
    (if n'
      [n' c' gen' nil]
      (parse-triples s n c gen))))
