(ns ^{:doc "A namespace for manually parsing Turtle. Entry point is parse-doc."
      :author "Paula Gearon"}
    quoll.raphael.core
  (:require [clojure.string :as str]
            [quoll.raphael.m :refer [throw-unex] :include-macros true]
            [quoll.raphael.text :as text :refer [char-at]]
            [quoll.raphael.triples :as triples]
            [tiara.data :refer [EMPTY_MAP]])
  #?(:clj (:import [clojure.lang ExceptionInfo])))

(def RDF "http://www.w3.org/1999/02/22-rdf-syntax-ns#")

(def ^:dynamic *loc* (volatile! [1 0]))

(defn reset-pos! [] (vreset! *loc* [0 0]))

(defn update-pos!
  [n]
  (vswap! *loc* (fn [loc] [(inc (nth loc 0)) n])))

(defprotocol IRI
  (as-iri-string [iri generator] "Returns this object as an iri string."))

(defprotocol NodeGenerator
  (new-node [generator] [generator label]
    "Generate a new node, optionally with a label indicating a reusable node. Return the next generator and node")
  (add-base [generator iri]
    "Adds a base iri for the document")
  (add-prefix [generator prefix iri]
    "Adds a prefix/iri pair to the namespace map")
  (iri-for [generator prefix]
    "Gets the stored iri for a given prefix")
  (get-namespaces [generator]
    "Returns a map of all the namespaces recorded by this generator")
  (get-base [generator]
    "Returns the base of this generator, if one has been set")
  (new-qname [generator prefix local]
    "Returns a Qualified Name object.")
  (new-iri [generator iri]
    "Returns an IRI object.")
  (new-literal [generator s] [generator s t]
    "Returns a literal. Either simple, or with a type")
  (new-lang-string [generator s l]
    "Returns a string literal, with a language tag")
  (rdf-type [generator] "Returns the rdf:type qname")
  (rdf-first [generator] "Returns the rdf:first qname")
  (rdf-rest [generator] "Returns the rdf:rest qname")
  (rdf-nil [generator] "Returns the rdf:nil qname"))

(defrecord BlankNode [n]
  Object
  (toString [this] (str "_:b" n)))

(defrecord Iri [prefix local iri]
  IRI
  (as-iri-string [this generator] (or iri (str (iri-for generator prefix) local)))
  Object
  (toString [this] (if prefix
                     (str prefix ":" local)
                     (str "<" iri ">"))))

(extend-protocol IRI
  #?(:clj String :cljs string)
  (as-iri-string [this generator] this))

(def RDF-TYPE (->Iri "rdf" "type" "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
(def RDF-FIRST (->Iri "rdf" "first" "http://www.w3.org/1999/02/22-rdf-syntax-ns#first"))
(def RDF-REST (->Iri "rdf" "rest" "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"))
(def RDF-NIL (->Iri "rdf" "nil" "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))

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

(defn print-escape
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
      lang (str (print-escape value) "@" lang)
      type (str (print-escape value) "^^" (iri-string type))
      :default (print-escape value))))

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
  (add-base [this iri]
    (update this :namespaces assoc :base (as-iri-string iri this)))
  (add-prefix [this prefix iri]
    (update this :namespaces assoc prefix (as-iri-string iri this)))
  (iri-for [this prefix]
    (get namespaces prefix))
  (get-namespaces [this]
    (dissoc namespaces :base))
  (get-base [this]
    (:base namespaces))
  (new-qname [this prefix local]
    (->Iri prefix local (str (get namespaces prefix) local)))
  (new-iri [this iri]
    (->Iri nil nil iri))
  (new-literal [this s]
    (->Literal s nil nil))
  (new-literal [this s t]
    (->Literal s nil t))
  (new-lang-string [this s lang]
    (->Literal s lang nil))
  (rdf-type [this] RDF-TYPE)
  (rdf-first [this] RDF-FIRST)
  (rdf-rest [this] RDF-REST)
  (rdf-nil [this] RDF-NIL))

(defn new-generator [] (->Generator 0 {} EMPTY_MAP))

(defn add-range
  "Adds a range of characters into set.
  chars - the set of characters.
  low - the low end of the character range to add, inclusive.
  high - the high end of the character range to add, inclusive.
  return - the set with the new range of characters added."
  [chars low high]
  (into chars (map char) (range (text/char-code low) (inc (text/char-code high)))))

(def whitespace? #{\space \tab \return \newline})

(def hex? (-> #{} (add-range \0 \9) (add-range \A \F) (add-range \a \f)))

(def pn-chars-base?
  (-> #{}
      (add-range \A \Z) (add-range \a \z) (add-range \u00C0 \u00D6) (add-range \u00D8 \u00F6)
      (add-range \u00F8 \u02FF) (add-range \u0370 \u037D) (add-range \u037F \u1FFF)
      (add-range \u200C \u200D) (add-range \u2070 \u218F) (add-range \u2C00 \u2FEF)
      (add-range \u3001 \uD7FF) (add-range \uF900 \uFDCF) (add-range \uFDF0 \uFFFD)))

;; (range 0x10000 0xEFFFF) will be taken care of by the high/low surrogate tests

(def pn-chars-u? (conj pn-chars-base? \_))

(def pn-chars-ud? (add-range pn-chars-u? \0 \9))

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
(def end-comment? #{\newline :eof})

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
      [n :eof]
      (if (whitespace? c)
        (let [n' (inc n)]
          (when (= \newline c) (update-pos! n))
          (recur n' (char-at s n')))
        (if (= \# c)
          (let [n' (loop [nn (inc n)]
                     (let [cc (char-at s nn)
                           nn' (inc nn)]
                       (if (end-comment? cc)
                         (do
                           (when (= \newline cc) (update-pos! nn))
                           nn')
                         (recur nn'))))]
            (recur n' (char-at s n')))
          [n c])))))

(defn skip-to
  "Skip over the whitespace to the required character. If there are nonwhitespace characters, this is an error.
  s - The string to read.
  n - The starting position.
  c - The character at position n.
  chars - the set of chars to skip to.
  return: [n c]
  n - after skipping whitespace the new offset immediately after the requested character.
  c - the first character after the requested character, found at position n"
  [s n c chars]
  (loop [n n c c]
    (cond
      (chars c) (let [n' (inc n)]
                  [n' (char-at s n')])
      (whitespace? c) (let [n' (inc n)]
                        (when (= \newline c) (update-pos! n))
                        (recur n' (char-at s n')))
      (= \# c) (let [n' (loop [nn (inc n)]
                          (let [cc (char-at s nn)
                                nn' (inc nn)]
                            (if (end-comment? cc)
                              (do
                                (when (= \newline cc) (update-pos! nn))
                                nn')
                              (recur nn'))))]
                 (recur n' (char-at s n')))
      :default (throw-unex *loc* "Unexpected characters after end of line: " s n))))

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
                   (throw-unex *loc* "Unexpected '.' at end of prefix: " s n)
                   (let [nn' (inc n')]
                     [nn' (char-at s nn') (str sb)]))
        (= n n') (cond
                   (pn-chars-base? c) (let [n' (inc n')]
                                        (text/append! sb c)
                                        (recur n' (char-at s n')))
                   (text/high-surrogate? c) (let [nn (+ n' 2)
                                                  c2 (char-at s (inc n'))]
                                              (when-not (text/low-surrogate? c2)
                                                (throw-unex *loc* "Bad Unicode characters at start of prefix: " s n))
                                              (text/append! sb c)
                                              (text/append! sb c2)
                                              (recur nn (char-at s nn)))
                   :default (throw-unex *loc* "Unexpected character at start of prefix: " s n))
        (pn-chars? c) (let [n' (inc n')]
                        (text/append! sb c)
                        (recur n' (char-at s n')))
        (text/high-surrogate? c) (let [nn (+ n' 2)
                                       c2 (char-at s (inc n'))]
                                   (when-not (text/low-surrogate? c2)
                                     (throw-unex *loc* "Bad Unicode characters in prefix: " s n))
                                   (text/append! sb c)
                                   (text/append! sb c2)
                                   (recur nn (char-at s nn)))
        :default (throw-unex *loc* (str "Unexpected character '" c "' (" (text/char-code c) ") in prefix: ") s n)))))

(defn parse-u-char
  "Parse a an unescapped code of uxxxx or Uxxxxxxxx. A preceding \\ character was just parsed.
  s - the string to parse.
  n - the offset within the string to parse from.
  f - the character found at position n. Must be u or U.
  return: [n char]
  n - the offset immediately after the ucode
  char - the character code that was parsed"
  [s n f]
  (case f
    \u (let [end (+ n 5)]
         [end (char-at s end) (char (text/parse-hex (subs s (inc n) end)))])
    \U (let [end (+ n 9)
             unicode (text/parse-hex (subs s (inc n) end))
             [high low] (text/surrogates unicode)]
         [end (char-at s end) (str (char high) (char low))])
    (throw-unex *loc* "Unexpected non-U character when processing unicode escape" s n)))

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
  triples - the current triples
  return: [n c iri gen triples]
  n - The offset immediately after the prefix.
  c - the character at offset n.
  iri - The iri string.
  gen - the generator.
  triples - the current triples."
  [s n c gen triples]
  (when-not (= c \<)
    (throw-unex *loc* "Unexpected character commencing an IRI Reference: " s n))
  (let [sb (text/string-builder)]
    (loop [n (inc n) c (char-at s n)]
      (if (= c \>)
        (let [i (str sb)
              iri (new-iri gen (if (relative-iri? i)
                                 (if-let [base (get-base gen)] (str base i) i)
                                 i))
              n' (inc n)]
          [n' (char-at s n') iri gen triples])
        (if (non-iri-char? c)
          (throw-unex *loc* "Unexpected character in IRI: " s n)
          (if (= c \\)
            (if-let [[n' c' ch] (let [nn (inc n)] (parse-u-char s nn (char-at s nn)))]
              (do
                (text/append! sb ch)
                (recur n' c'))
              (throw-unex *loc* "Unexpected \\ character in IRI: " s n))
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
                            (throw-unex *loc* "Bad escape code in localname: " s n)))
                     \\ (let [a (char-at s (inc n))]
                          (if (local-esc? a)
                            (do
                              (text/append! sb a)
                              (+ n 2))
                            (throw-unex *loc* "Bad escape code in localname: " s n)))
                     (do
                       (text/append! sb c)
                       (inc n))))
        f (char-at s n)
        _ (when-not (local-chars? f) (throw-unex *loc* (str "Unexpected character '" f "' in local name: ") s n))
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
  pre - Optional. The initial part of the string that has already been parsed as a string builder.
  c - the first character of the prefixed name.
  gen - the generator
  triples - the current triples.
  return: [n c prefix]
  n - The offset immediately after the prefixed name.
  c - The character immediately after the prefixed name.
  qname - The prefixed name as a Iri.
  gen - the updated generator.
  triples - the triples generated in parsing the node."
  ([s n c gen triples]
   (when-not (or (pn-chars-base? c) (= \: c))
     (throw-unex *loc* "Prefix char starts with illegal character" s n))
   (parse-prefixed-name s n nil c gen triples))
  ([s n pre c gen triples]
   (let [sb (or pre (text/string-builder))
         ([
         [n prefix] (loop [n n c c dot false]
                      (if (= \: c)
                        (if dot
                          (throw-unex *loc* "Prefix illegally ends with a '.': " s n)
                          [(inc n) (str sb)])
                        (if (pn-chars-dot? c)
                          (let [n' (inc n)]
                            (text/append! sb c)
                            (recur n' (char-at s n') (dot? c)))
                          (if (and (whitespace? c) (= "a" (str sb)))
                            [(inc n) nil]
                            (throw-unex *loc* (str "Illegal character '" c "' in prefix: ") s n)))))]
     (if prefix
       (let [[n c local] (parse-local s n)]
         [n c (new-qname gen prefix local) gen triples])
       [n (char-at s n) (rdf-type gen) gen triples]))))

(defn parse-iri
  "Parse an iri.
  s - the string to parse.
  n - the offset to parse from.
  c - the char found at position n.
  gen - the generator to use.
  triples - the current triples.
  return: [n c iri]
  n - the offset immediately after the iri.
  c - the character at offset n.
  iri - the node for the parsed iri. Either an IRI string or an Iri.
  gen - the updated generator.
  triples - the triples generated in parsing the node."
  [s n c gen triples]
  (if (= \< c)
    (parse-iri-ref s n c gen triples)
    (parse-prefixed-name s n c gen triples)))

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
  value - the unescaped character."
  [s n c]
  (if-let [e (echars c)]
    (let [n' (inc n)]
      [n' (char-at s n') e])
    (if (#{\u \U} c)
      (parse-u-char s n c)
      (throw-unex *loc* (str "Unexpected escape character <" c "> found in literal: ") s n))))

(defn parse-long-string
  "Parse a triple-quoted string form. Because this is already identified as a triple quote
  the offset of n and the character represent the first character after the quotes.
  end-q - the ending quote character to terminate on.
  s - the string to parse.
  n - the offset to parse from. After the quotes.
  c - the char found at position n.
  c - the char found at position n.
  gen - the node generator.
  return: [n c value]
  n - the offset immediately after the closing quotes.
  c - the character at offset n.
  value - the parsed string. "
  [end-q s n c]
  (let [sb (text/string-builder)]
    (loop [n n esc false current (char-at s n)]
      (cond
        (= end-q current)
        (if esc
          (throw-unex *loc* "Unexpected escape sequence in long-form string: " s (dec n))
          (let [n' (inc n)
                next-char (char-at s n')
                n2 (inc n')
                c2 (char-at s n2)]
            (if (= end-q next-char) 
              (let [n3 (inc n2)
                    c3 (char-at s n3)]
                (if (= end-q c2)
                  [n3 c3 (str sb)]
                  (do  ;; third character was not a third quote
                    (text/append! sb current)
                    (text/append! sb next-char)
                    (if (= \\ c2)
                      (recur n3 true c3)
                      (do
                        (when (= \newline current) (update-pos! n))
                        (text/append! sb c2)
                        (recur n3 false c3))))))
              (do  ;; second character was not a second quote
                (text/append! sb current)
                (if (= \\ next-char)
                  (recur n2 true c2)
                  (do
                    (when (= \newline current) (update-pos! n))
                    (text/append! sb next-char)
                    (recur n2 false c2)))))))

        (= :eof current)
        (throw-unex *loc* "Improperly terminated long literal: " s n)

        :default
        (if esc
          (let [[n2 c2 ecode] (escape s n current)]
            (text/append! sb ecode)
            (recur n2 false c2))
          (let [n' (inc n)
                next-char (char-at s n')]
            (if (= \\ current)
              (recur n' true next-char)
              (do
                (text/append! sb current)
                (recur n' false next-char)))))))))

(defn parse-string
  "Parse a single-quoted string form.
  end-q - the ending quote character to terminate on.
  s - the string to parse.
  n - the offset to parse from.
  c - the char found at position n. This is after the opening quote character.
  gen - the node generator.
  triples - the current triples.
  return: [n c value]
  n - the offset immediately after the subject.
  c - the character at offset n.
  value - the parsed string. "
  [end-q s n c]
  (let [sb (text/string-builder)]
    (loop [n n esc false current c]
      (cond
        (= end-q current) ;; end of the string, unless escaped
        (let [n' (inc n)
              next-char (char-at s n')]
          (if esc
            (do
              (text/append! sb current)
              (recur n' false next-char))
            [n' next-char (str sb)]))

        (= :eof current)
        (throw-unex *loc* "Improperly terminated literal: " s n)

        :default
        (if esc
          (let [[n2 c2 ecode] (escape s n current)]
            (text/append! sb ecode)
            (recur n2 false c2))
          (let [n' (inc n)
                next-char (char-at s n')]
            (if (= \\ current)
              (recur n' true next-char)
              (do
                (when (= \newline current) (update-pos! n))
                (text/append! sb current)
                (recur n' false next-char)))))))))

(defn parse-literal
  "Parse a literal that starts with a quote character. This also includes the
  triple quote form that allows for raw unescaped strings.
  s - the string to parse.
  n - the offset to parse from.
  c - the char found at position n. This is a quote: either ' or \"
  gen - the node generator.
  triples - the current triples.
  return: [n c value]
  n - the offset immediately after the subject.
  c - the character at offset n.
  value - the parsed value, in string form if it is plain.
  gen - the updated generator.
  triples - the triples generated in parsing the node."
  [s n c gen triples]
  (let [n' (inc n)
        c' (char-at s n')
        startlong (+ 2 n)
        [n c lit-str] (if (= c' c)
                        (let [n2 (inc n')
                              c2 (char-at s n2)]
                          (if (= c2 c)
                            (let [n3 (inc n2)]
                              (parse-long-string c s n3 (char-at s n3)))
                            [n2 c2 ""]))
                        (parse-string c s n' c'))]
    (case c
      \^ (if (= \^ (char-at s (inc n)))
           (let [n2 (+ n 2)
                 [n' c' iri gen triples] (parse-iri s n2 (char-at s n2) gen triples)]
             [n' c' (new-literal gen lit-str iri) gen triples])
           (throw-unex *loc* "Badly formed type expression on literal. Expected ^^: " s n))
      \@ (let [n' (inc n)]
           (if-let [[lang] (re-find #"^[a-zA-Z]+(-[a-zA-Z0-9]+)*" (subs s n'))]
             (let [end (+ n' (count lang))]
               [end (char-at s end) (new-lang-string gen lit-str lang) gen triples])
             (throw-unex *loc* "Bad language tag on literal: " s n')))
      [n c lit-str gen triples])))

(def end-mantissa? #{\e \E :eof})

(defn parse-number
  "Parse a numeric literal.
  s - the string to parse.
  n - the offset to parse from.
  c - the char found at position n.
  gen - the node generator.
  triples - the current triples.
  return: [n c value]
  n - the offset immediately after the subject.
  c - the character at offset n.
  value - the parsed number.
  gen - the updated generator
  triples - the triples generated in parsing the node."
  [s n c gen triples]
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
            (throw-unex *loc* (str "Invalid number: '" full-nr "' in:") s n))
        nr (if (or (= \. (text/last-char-str up-to-dot)) (re-find #"[eE]" after-dot))
             (parse-double full-nr)
             (parse-long full-nr))]
    [n' nextc nr gen triples]))

(declare parse-predicate parse-object)

(defn parse-collection
  "Parses a collection. This creates a linked list in the triples.
  s - The string to parse.
  n - The offset to parse from.
  c - the first character of the collection
  gen - the current generator
  triples - the current triples
  return: [n c node gen triples]
  n - The offset immediately after the prefix.
  c - the character at offset n.
  node - The node representing the collection.
  gen - the generator.
  triples - the current triples."
  [s n c gen triples]
  (let [n' (inc n)
        [n c] (skip-whitespace s (inc n) (char-at s n'))
        rfirst (rdf-first gen)
        rrest (rdf-rest gen)
        rnil (rdf-nil gen)]
    (if (= \) c)
      (let [n' (inc n)]
        [n' (char-at s n') rnil gen triples])
      (let [[gen head] (new-node gen)]
        (loop [last-node head [n c node gen triples] (parse-object s n c gen triples)]
          (let [triples (triples/append! triples last-node rfirst node)
                [n c] (skip-whitespace s n c)]
            (if (= \) c)
              (let [n' (inc n)
                    triples (triples/append! triples last-node rrest rnil)]
                [n' (char-at s n') head gen triples])
              (let [[gen node] (new-node gen)
                    triples (triples/append! triples last-node rrest node)]
                (recur node (parse-object s n c gen triples))))))))))

(defn parse-blank-node
  "Parses a blank node label.
  s - The string to parse.
  n - The offset to parse from.
  c - the first character of the blank node
  gen - the current generator
  triples - the current triples
  return: [n c node gen triples]
  n - The offset immediately after the prefix.
  c - the character at offset n.
  node - The blank node.
  gen - the generator.
  triples - the current triples."
  [s n c gen triples]
  (when-not (= \: (char-at s (inc n)))
    (throw-unex *loc* "Illegal underscore (_) at start of symbol: " s n))
  (let [c (char-at s (+ n 2))
        _ (when-not (pn-chars-ud? c)
            (throw-unex *loc* "Illegal character at start of blank node label: " s n))
        sb (text/string-builder)
        _ (text/append! sb c)
        [n c label] (loop [n (+ 3 n) c (char-at s n) dot false]
                      (if (pn-chars-dot? c)
                        (let [n' (inc n)]
                          (text/append! sb c)
                          (recur n' (char-at s n') (dot? c)))
                        (if dot
                          (throw-unex *loc* "blank node illegally ends with a '.': " s n)
                          [n c (str sb)])))
        [gen node] (new-node gen label)]
    [n c node gen triples]))

(def end-list? #{\] \.})

(defn maybe-parse-predicate
  "Parses an IRI as a predicate if one is available. Otherwise return a nil as the IRI.
  s - The string to parse.
  n - The offset to parse from.
  c - the first character of the iri reference.
  gen - the current generator
  triples - the current triples
  return: [n c iri gen triples]
  n - The offset immediately after the prefix.
  c - the character at offset n.
  iri - The iri string.
  gen - the generator.
  triples - the current triples."
  [s n c gen triples]
  (if (end-list? c)
    [n c nil gen triples]
    (parse-iri s n c gen triples)))

(defn parse-predicate-object-list
  "Parse a predicate-object list
  s - The string to parse from
  n - The offset in the string to start at.
  c - The character at position n
  gen - The generator for blank nodes and namespace resolution
  triples - An accumulating transient of triples.
  return [n c gen triples]
  n - the new parse position
  c - the character at position n
  gen - the updated generator
  triples - the updated triples sequence."
  [s n c subject gen triples]
  (loop [[n c pred gen triples :as all] (maybe-parse-predicate s n c gen triples)]
    (if-not pred
      [n c gen triples]
      (let [[n c] (skip-whitespace s n c)
            [n c gen triples] (loop [[n c obj gen triples] (parse-object s n c gen triples)]
                                (let [[n c] (skip-whitespace s n c)
                                      triples (triples/append! triples subject pred obj)]
                                  (case c
                                    (\] \. \;) [n c gen triples]
                                    \, (let [n' (inc n)
                                             [n c] (skip-whitespace s n' (char-at s n'))]
                                         (if-let [parse-result (try
                                                                 (parse-object s n c gen triples)
                                                                 (catch ExceptionInfo e nil))]
                                           (recur parse-result)
                                           [n c gen triples]))
                                    (throw-unex *loc* "Unexpected separator in predicate-object list: " s n))))]
        (if (= c \;)
          (let [n' (inc n)
                [n c] (skip-whitespace s n' (char-at s n'))]
            (recur (maybe-parse-predicate s n c gen triples)))
          [n c gen triples])))))

(defn anon-blank-node
  "Generates a new blank node with no properties. Already passed the opening [ character, and whitespace.
  This function just steps to the next position.
  s - The string to parse. Not read by this function.
  n - The position in the string.
  c - The character at position n. This must be a ]
  g - The generator.
  triples - An accumulating transient of triples.
  return: [n c node gen triples more?]
  n - The next character position after the blank node.
  c - the character found at n
  node - the new anonymous blank node
  gen - the updated generator
  triples - the updated triples sequence
  enf? - If this is a subject, then is this is not enough and properties are required. Always false."
  [s n c g triples]
  (let [n' (inc n)
        [g node] (new-node g)]
    [n' (char-at s n') node g triples false]))

(defn parse-blank-node-entity
  "Parse a blank node property/value list. Already past the opening [ character and whitespace.
  s - The string to parse from
  n - The offset in the string to start at.
  c - The character at position n
  gen - The generator for blank nodes and namespace resolution
  triples - An accumulating transient of triples.
  return [n c node gen triples]
  n - the new parse position
  c - the character at position n
  node - the root blank node that is being parsed
  gen - the updated generator
  triples - the updated triples sequence
  enf? - is this enough if this is a subject? A predicateObject sequence is not needed. true."
  [s n c gen triples]
  (let [[gen node] (new-node gen)
        [n c gen triples] (parse-predicate-object-list s n c node gen triples)
        [n c] (skip-whitespace s n c)]
    (if (= c \])
      (let [n' (inc n)]
        [n' (char-at s n') node gen triples true])
      (throw-unex *loc* "Structured blank node entity improperly terminated: " s n))))

(defn parse-subject
  "Parse a subject entity, including any triples.
  s - the string to parse.
  n - the offset to parse from.
  c - the char found at position n.
  gen - the node generator.
  triples - the current triples.
  return: [n c subject gen triples]
  n - the offset immediately after the subject.
  c - the character at offset n.
  subject - the node for the parsed subject.
  gen - the updated generator
  triples - the triples generated in parsing the node.
  enf? - indicates if this subject is enough and a predicateObject list is not needed.
         Most types return nil for this (falsey)."
  [s n c gen triples]
  (case c
    \< (parse-iri-ref s n c gen triples)
    \( (parse-collection s n c gen triples)
    \_ (parse-blank-node s n c gen triples)
    \[ (let [n' (inc n)
             [n c] (skip-whitespace s n' (char-at s n'))]
         (if (= \] c)
           (anon-blank-node s n c gen triples)
           (parse-blank-node-entity s n c gen triples)))
    (parse-prefixed-name s n c gen triples)))

(defn parse-ambiguous-elt
  "Reads an object to the point where ambiguity can be resolved.
  Processes text as either a prefixed name or the text being searched on.
  s - The string to parse.
  n - The position in the string to parse from.
  c - the character at position n.
  gen - The current generator.
  triples - The current triples.
  common-text - The text that can start either type of line.
  non-iri-op - The function to use to parse the line if it does not contain triples.
                   This function accepts the string and offset into the string
  return: [n c gen triples]
  n - the new offset after parsing.
  c - the character at position n.
  gen - the next generator state.
  obj - the object generated from parsing this object."
  [s n c gen triples common-text non-iri-op]
  (let [common-len (count common-text)
        text (text/string-builder (subs common-text 0 1))]
    (loop [a 1 cp (char-at s (inc n))]
      (if (and (< a common-len) (= (text/char-at common-text a) (text/lower-case-char cp)))
        (let [na (inc a)]
          (text/append! text cp)
          (recur na (char-at s (+ n na))))
        (let [nn (+ n a)]
          (if (or (pn-chars? cp) (= \: cp))
            (parse-prefixed-name s nn text cp gen triples)
            (non-iri-op)))))))

(defn parse-object
  "Parse an object entity, including any triples.
  s - the string to parse.
  n - the offset to parse from.
  c - the char found at position n.
  gen - the node generator.
  triples - the current triples.
  return: [n c object gen triples]
  n - the offset immediately after the object.
  c - the character at offset n.
  object - the node for the parsed object.
  gen - the updated generator.
  triples - the triples generated in parsing the node.
  Blank node entities can also return a true at the end of the vector, but this should be ignored."
  [s n c gen triples]
  (case c
    \< (parse-iri-ref s n c gen triples)
    \( (parse-collection s n c gen triples)
    \_ (parse-blank-node s n c gen triples)
    \[ (let [n' (inc n)
             [n c] (skip-whitespace s n' (char-at s n'))]
         (if (= \] c)
           (anon-blank-node s n c gen triples)
           (parse-blank-node-entity s n c gen triples)))
    (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \. \+ \-) (parse-number s n c gen triples)
    (\' \") (parse-literal s n c gen triples)
    \f (parse-ambiguous-elt s n c gen triples "false"
                            (fn [] (let [n' (+ n 5)]
                                     [n' (char-at s n') false gen triples])))
    \t (parse-ambiguous-elt s n c gen triples "true"
                            (fn [] (let [n' (+ n 4)]
                                     [n' (char-at s n') true gen triples])))
    (parse-prefixed-name s n c gen triples)))

(defn parse-triples
  "Parse a top level triples from a string.
  s - the string to parse from.
  n - the offset in the string to retrieve from.
  c - the character found at position n.
  gen - the generator to use for blank nodes.
  triples - A transient vector of triples.
  return: [n c gen triples]
  n - the new offset after parsing.
  c - the character at offset n.
  gen - the next generator state.
  triples - the triples generated from parsing this line."
  [s n c gen triples]
  (let [initial-count (count triples)
        [n c] (skip-whitespace s n c)]
    (if (= :eof c)
      [n :eof gen triples]
      (let [[n c subject gen triples enf?] (parse-subject s n c gen triples)
            [n c] (skip-whitespace s n c)
            [n c gen triples] (parse-predicate-object-list s n c subject gen triples)]
        (when-not (= \. c)
          (throw-unex *loc* "Statements invalidly terminated: " s n))
        (when (and (not enf?) (= initial-count (count triples)))
          (throw-unex *loc* "Subjects require predicates and objects: " s n))
        (let [n' (inc n)]
          [n' (char-at s n') gen triples])))))

(defn pre-parse-triples
  "Parse a top level triples from a string,
  with some of the first triple already parsed as a prefixed stirng.
  s - the string to parse from.
  n - the offset in the string to retrieve from.
  c - the character found at position n.
  gen - the generator to use for blank nodes.
  triples - A transient vector of triples.
  pre - the previously read prefix of the line in a string builder
  return: [n c gen triples]
  n - the new offset after parsing.
  c - the character at offset n.
  gen - the next generator state.
  triples - the triples generated from parsing this line."
  [s n c gen triples pre]
  (let [initial-count (count triples)]
    (if (= :eof c)
      (throw-unex *loc* "Unexpected termination while parsing a prefixed line: " s n)
      (let [[n c subject gen triples enf?] (parse-prefixed-name s n pre c gen triples)
            [n c] (skip-whitespace s n c)
            [n c gen triples] (parse-predicate-object-list s n c subject gen triples)]
        (when-not (= \. c)
          (throw-unex *loc* "Statements invalidly terminated: " s n))
        (when (and (not enf?) (= initial-count (count triples)))
          (throw-unex *loc* "Subjects require predicates and objects: " s n))
        (let [n' (inc n)]
          [n' (char-at s n') gen triples])))))

(defn parse-prefix-iri-end
  "Parse an iri and a newline for a PREFIX or BASE directive.
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
            [n c iri] (parse-iri-ref s n c gen nil)
            [n c] (skip-to s n c end-char)]
        [n c (add-prefix gen prefix iri)])
      (throw-unex *loc* "Unknown statement: " s n))))

(defn parse-base-end
  "Parse an iri and the end-char for the end of the line.
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
            ;; nil triples, since triples are not being generated during a directive
            [n c iri] (parse-iri-ref s n c gen nil)
            [n c] (skip-to s n c end-char)]
        [n c (add-base gen iri)])
      (throw-unex *loc* "Unknown statement: " s n))))

(defn parse-ambiguous-line
  "Reads a line up to the point where ambiguity can be resolved.
  Processes the line as either the beginning of triples, or a provided operation.
  s - The string to parse.
  n - The position in the string to parse from.
  c - the character at position n. (Optional: can be inferred)
  gen - The current generator.
  triples - The current triples.
  common-text - The text that can start either type of line.
  non-triples-op - The function to use to parse the line if it does not contain triples.
                   This function accepts the string and offset into the string
  return: [n c gen triples]
  n - the new offset after parsing.
  c - the character at position n.
  gen - the next generator state.
  triples - the triples generated from parsing this line."
  [s n c gen triples common-text non-triples-op]
  (let [common-len (count common-text)
        text (text/string-builder)]
    (loop [a 0 cp c]
      (if (and (< a common-len) (= (text/char-at common-text a) (text/lower-case-char cp)))
        (let [na (inc a)]
          (text/append! text cp)
          (recur na (char-at s (+ n na))))
        (let [nn (+ n a)]
          (if (whitespace? cp) ;; should only occur when the common length was reached
            (non-triples-op s nn)
            (pre-parse-triples s nn cp gen triples text)))))))

(defn parse-statement
  "Parse a directive or triples.
  s - The string to parse.
  n - The position in the string to parse from.
  c - the character at position n. (Optional: can be inferred)
  gen - The current generator.
  triples - The current triples.
  return: [n c gen triples]
  n - the new offset after parsing.
  c - the character at position n.
  gen - the next generator state.
  triples - the triples generated from parsing this line."
  ([s n gen triples] (parse-statement s n (char-at s n) gen triples))
  ([s n c gen triples]
   (let [[n c] (skip-whitespace s n c)
         [n' c' gen' triples'] (case c
                                 \@ (let [text (text/ssubs s (inc n) (+ n 5))]
                                      (if (= text "base")
                                        (parse-base-end s n gen dot? 5)
                                        (let [text2 (str text (text/ssubs s (+ n 5) (+ n 7)))]
                                          (if (= text2 "prefix")
                                            (parse-prefix-iri-end s n gen dot? 7)
                                            (throw-unex *loc* "Unknown statement: " s n)))))
                                 (\B \b) (parse-ambiguous-line s n c gen triples "base"
                                                               #(parse-base-end %1 %2 gen newline? 0))
                                 (\P \p) (parse-ambiguous-line s n c gen triples "prefix"
                                                               #(parse-prefix-iri-end %1 %2 gen newline? 0))
                                 :eof [n c gen triples]
                                 nil)]
     (if n'
       [n' c' gen' (or triples' triples)]
       (parse-triples s n c gen triples)))))

(defn parse
  "parse a string as a turtle document
  s - the stirng containing the document.
  g - an implementation of the Generator protocol for assigning blank nodes, IRIs and Literals,
      and managing namespaces. Optional.
  return: {:base <optional IRI>
           :namespaces <prefixes mapped to IRIs>
           :triples <vector of 3 element vectors>}"
  ([s] (parse s (new-generator)))
  ([s generator]
   (reset-pos!)
   (let [triples (triples/triple-accumulator)
         [n gen triples] (binding [*loc* (volatile! [1 0])]
                           (loop [[n c gen triples] (parse-statement s 0 generator triples)]
                             (if (= :eof c)
                               [n gen triples]
                               (recur (parse-statement s n c gen triples)))))
         result {:namespaces (get-namespaces gen)
                 :triples (seq triples)}
         base (get-base gen)]
     (if base
       (assoc result :base base)
       result))))
