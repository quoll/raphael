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

(defn iri-string
  "Converts an IRI to a string form for printing"
  [iri-ref]
  (if (string? iri-ref)
    (str \< iri-ref \>)
    (str iri-ref)))

(def echar-map {\newline "\\n"
                \return "\\r"
                \tab "\\t"
                \formfeed "\\f"
                \backspace "\\b"
                \" "\\\""
                \\ "\\\\"})

(defn escape
  "Escapes a string for printing"
  [s]
  (str \"
       (-> s
           (str/replace #"[\n\r\t\f\"\\]" #(echar-map (char-at % 0)))
           (str/replace "\b" "\\b"))
       \"))

(defrecord Literal [value lang type]
  Object
  (toString [this]
    (cond
      lang (str (escape value) \@ lang)
      type (str (escape value) "^^" (iri-string type))
      :default (escape value))))

(defn new-literal
  "Creates a new literal object"
  ([value] (->Literal value nil nil))
  ([value type]
   (->Literal value nil type)))

(defn new-lang-string
  "Creates a language-tagged literal"
  [value lang]
  (->Literal value lang nil))

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
  c - The character at position n.
  return: [n c]
  n - the new offset after skipping whitespace.
  c - the first non-whitespace character, found at position n"
  [s n c]
  (loop [n n c c]
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
  c - The character at position n.
  chars - the set of chars to skip to.
  return: [n c]
  n - the new offset after skipping whitespace to the end of the line.
  c - the first non-whitespace character, found at position n"
  [s n c chars]
  (loop [n n c c]
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
  [s n c]
  (let [[n c] (skip-whitespace s n c)]
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
  "Parse a an unescapped code of uxxxx or Uxxxxxxxx. A preceding \\ character was just parsed.
  s - the string to parse.
  n - the offset within the string to parse from.
  f - the character found at position n. Must be u or U.
  return: [n char]
  n - the offset immediately after the ucode
  char - the character code that was parsed"
  [s n f]
  (let [end (case f
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
            (if-let [[n ch] (let [nn (inc n)] (parse-u-char s nn (char-at s nn)))]
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
        add-char (fn [c n] ;; adds a char, looking ahead if this is an escape sequence
                   (case c
                     \% (let [a (char-at s (inc n))
                              b (char-at s (+ n 2))]
                          (if (and (hex? a) (hex? b))
                            (do
                              (text/append! sb c)
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
                     (do
                       (text/append! sb c)
                       (inc n))))
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

(defn parse-iri
  "Parse an iri.
  s - the string to parse.
  n - the offset to parse from.
  c - the char found at position n.
  gen - the generator to use.
  return: [n c iri]
  n - the offset immediately after the iri.
  c - the character at offset n.
  iri - the node for the parsed iri. Either an IRI string or a QName."
  [s n c gen]
  (if (= \< c)
    (parse-iri-ref s n c gen)
    (parse-prefixed-name s n c gen)))

(def echars "Maps ascii characters into their escape code"
  {\b \backspace
   \t \tab
   \n \newline
   \r \return
   \f \formfeed
   \\ \\})

(defn escape
  "Reads an escaped code from the input starting at the current position.
  s - the string to parse
  n - the position of the beginning of the already escaped sequence (after the \\ character)
  c - the character at position n
  return: [n c value]
  n - the position immediately following the escaped sequence
  c - the character at n
  value - the unescaped character"
  [s n c]
  (if-let [e (echars c)]
    (let [n' (inc n)]
      [n' (char-at s n') e])
    (if (#{\u \U} c)
      (parse-u-char s n c)
      (throw-unex (str "Unexpected escape character '" c "' found in literal") s n))))

(defn parse-long-string
  "Parse a triple-quoted string form. Because this is already identified as a triple quote
  the offset of n and the character represent the first character after the quotes.
  end-q - the ending quote character to terminate on.
  s - the string to parse.
  n - the offset to parse from. After the quotes.
  c - the char found at position n.
  return: [n c value]
  n - the offset immediately after the closing quotes.
  c - the character at offset n.
  value - the parsed string."
  [end-q s n c]
  (let [sb (text/string-builder)]
    (loop [n n esc false current (char-at s n)]
      (let [n' (inc n)
            next-char (char-at s n')]
        (if (= end-q current)
          (if esc
            (throw-unex "Unexpected escape sequence in long-form string: " s (dec n))
            (if (= end-q next-char) 
              (let [n2 (inc n')
                    c2 (char-at s n2)]
                (if (= end-q c2)
                  (let [n3 (inc n2)]
                    [n3 (char-at s n3) (str sb)])
                  (do
                    (text/append! sb \")
                    (text/append! sb \")
                    (recur n2 false c2))))
              (do
                (text/append! sb \")
                (recur n' false next-char))))
          (if esc
            (let [[n2 c2 ecode] (escape s n' next-char)]
              (text/append! sb ecode)
              (recur n2 false c2))
            (if (= \\ current)
              (recur n' true next-char)
              (do
                (text/append! sb next-char)
                (recur n' false next-char)))))))))

(defn parse-string
  "Parse a single-quoted string form.
  end-q - the ending quote character to terminate on.
  s - the string to parse.
  n - the offset to parse from.
  c - the char found at position n. This is after the opening quote character.
  return: [n c value]
  n - the offset immediately after the subject.
  c - the character at offset n.
  value - the parsed string."
  [end-q s n c]
  (let [sb (text/string-builder)]
    (loop [n n esc false current c]
      (let [n' (inc n)
            next-char (char-at s n')]
        (if (= end-q current) ;; end of the string, unless escaped
          (if esc
            (do
              (text/append! sb \")
              (recur n' false next-char))
            [n' next-char (str sb)])
          (if esc
            (let [[n'' c'' ecode] (escape s n' next-char)]
              (text/append! sb ecode)
              (recur n'' false c''))
            (if (= \\ current)
              (recur n' true next-char)
              (do
                (text/append! sb current)
                (recur n' false next-char)))))))))

(defn parse-literal
  "Parse a literal that starts with a quote character. This also includes the
  triple quote form that allows for raw unescaped strings.
  s - the string to parse.
  n - the offset to parse from.
  c - the char found at position n. This is a quote: either ' or \"
  return: [n c value]
  n - the offset immediately after the subject.
  c - the character at offset n.
  value - the parsed value, in string form if it is plain."
  [s n c gen]
  (let [n' (inc n)
        c' (char-at s n')
        startlong (+ 2 n)
        [n c lit-str] (if (= c' c)
                        (let [n2 (inc n')
                              c2 (char-at s n2)]
                          (if (= c2 c)
                            [n2 c2 ""]
                            (let [n3 (inc n2)]
                              (parse-long-string c s n3 (char-at s n3)))))
                      (parse-string c s n' c'))]
    (case c
      \^ (if (= \^ (char-at s (inc n)))
           (let [n2 (+ n 2)
                 [n' c' iri] (parse-iri s n2 (char-at s n2))]
             [n' c' (new-literal lit-str iri)])
           (throw-unex "Badly formed type expression on literal. Expected ^^: " s n))
      \@ (let [n' (inc n)]
           (if-let [[lang] (re-find #"^[a-zA-Z]+(-[a-zA-Z0-9]+)*" (subs s n'))]
             (let [end (+ n' (count lang))]
               [end (char-at s end) (new-lang-string lit-str lang)])
             (throw-unex "Bad language tag on literal: " s n')))
      [n c lit-str])))

(def end-mantissa? #{\e \E :eof})

(defn parse-number
  "Parse a numeric literal.
  s - the string to parse.
  n - the offset to parse from.
  c - the char found at position n.
  return: [n c value]
  n - the offset immediately after the subject.
  c - the character at offset n.
  value - the parsed number."
  [s n c]
  ;; at a minimum, up-to-dot will be populated by at least a sign, a digit, or a dot
  (let [up-to-dot (re-find #"[+-]?[0-9]*\.?" (subs s n))
        nd (+ n (count up-to-dot))
        [after-dot exp] (re-find #"^[0-9]*([eE][+-]?[0-9]+)?" (subs s nd))
        n' (+ nd (count after-dot))
        nextc (char-at s n')
        full-nr (subs s n n')
        ;; test here to avoid catching an exception in the core parser
        _ (when (let [frst (.charAt up-to-dot 0)]
                  (or (and (#{\+ \-} frst)
                           (let [sec (char-at full-nr 1)]
                             (or (end-mantissa? sec)
                                 (and (= \. sec) (end-mantissa? (char-at full-nr 2))))))
                      (and (= \. frst) (end-mantissa? (char-at full-nr 1)))
                      (and (nil? exp) (#{\e \E} nextc))))
            (throw-unex (str "Invalid number: '" full-nr "' in:") s n))
        nr (if (or (= \. (text/last-char up-to-dot)) (re-find #"[eE]" after-dot))
             (parse-double full-nr)
             (parse-long full-nr))]
    [n' nextc nr]))

(defn parse-collection
  [s n c gen]
  )

(defn parse-blank-node
  [s n c gen]
  )

(defn parse-blank-node-entity
  [s n c gen]
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
    (\' \") (parse-literal s n c gen)
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
        [n c] (skip-whitespace s n c)
        [n c gen] (parse-iri s n c gen)
        [n c] (skip-whitespace s n c)
        [n c gen object-triples] (parse-object s n c gen)
        [n c] (skip-past-dot s n)]
    [n c gen (concat subject-triples object-triples)]))

(defn parse-prefix-iri-end
  "Parse an iri and a newline.
  NOTE: THIS FUNCTION DOES NOT USE AN INCOMING CHARACTER
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
  (let [nskip (+ n skip)
        c (char-at s nskip)]
    (if (whitespace? c)
      (let [[n c] (skip-whitespace s nskip c)
            [n c prefix] (parse-prefix s n c)
            [n c] (skip-whitespace s n c)
            [n c iri] (parse-iri-ref s n c gen)
            [n c] (skip-to s n c end-char)]
        [n c (add-prefix gen prefix iri)])
      (throw-unex "Unknown statement: " s n))))

(defn parse-base-end
  "Parse an iri and a dot.
  NOTE: THIS FUNCTION DOES NOT USE AN INCOMING CHARACTER
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
  (let [nskip (+ n skip)
        c (char-at s nskip)]
    (if (whitespace? c)
      (let [[n c] (skip-whitespace s nskip c)
            [n c iri] (parse-iri-ref s n c nil)  ;; use a nil generator, since an existing base should not be used
            [n c] (skip-to s n c end-char)]
        [n c (add-prefix gen :base iri)])
      (throw-unex "Unknown statement: " s n))))

(defn parse-statement
  "Parse a directive or triples.
  s - The string to parse.
  n - The position in the string to parse from.
  c - the character at position n. (Optional: can be inferred)
  gen - The current generator.
  return: [n c gen triples]
  n - the new offset after parsing.
  c - the character at position n.
  gen - the next generator state.
  triples - the triples generated from parsing this line."
  ([s n gen] (parse-statement s n (char-at s n) gen))
  ([s n c gen]
   (let [[n c] (skip-whitespace s n c)
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
       (parse-triples s n c gen)))))
