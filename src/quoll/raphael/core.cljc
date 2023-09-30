(ns quoll.raphael.core
  "A namespace for manually parsing Turtle. Entry point is parse-doc."
  {:author "Paula Gearon"}
  (:require [clojure.string :as str]
            [quoll.raphael.m :refer [throw-unex] :include-macros true]
            [quoll.raphael.text :as text :refer [char-at]]
            [quoll.raphael.reader :as rdr :refer [get-char! position]]
            [quoll.raphael.triples :as triples]
            [tiara.data :refer [EMPTY_MAP]])
  #?(:clj (:import [clojure.lang ExceptionInfo])))

(def RDF "http://www.w3.org/1999/02/22-rdf-syntax-ns#")

(def ^:dynamic *loc* (volatile! [1 0]))

(defn reset-pos! [] (vreset! *loc* [0 0]))

(defn update-pos!
  [r]
  (vswap! *loc* (fn [loc] [(inc (nth loc 0)) (position r)])))

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

(defrecord IriRef [prefix local iri]
  IRI
  (as-iri-string [this generator] (or iri (str (iri-for generator prefix) local)))
  Object
  (toString [this] (if prefix
                     (str prefix ":" local)
                     (str "<" iri ">"))))

(extend-protocol IRI
  #?(:clj String :cljs string)
  (as-iri-string [this generator] this))

(def RDF-TYPE (->IriRef "rdf" "type" "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
(def RDF-FIRST (->IriRef "rdf" "first" "http://www.w3.org/1999/02/22-rdf-syntax-ns#first"))
(def RDF-REST (->IriRef "rdf" "rest" "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"))
(def RDF-NIL (->IriRef "rdf" "nil" "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))

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
    (->IriRef prefix local (str (get namespaces prefix) local)))
  (new-iri [this iri]
    (->IriRef nil nil iri))
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
  r - The reader
  c - The character at the current position.
  return: c
  c - the first non-whitespace character"
  [r c]
  (loop [c c]
    (if (= :eof c)
      :eof
      (if (whitespace? c)
        (do
          (when (= \newline c) (update-pos! r))
          (recur (get-char! r)))
        (if (= \# c)
          (do
            (loop []
              (let [cc (get-char! r)]
                (if (end-comment? cc)
                  (when (= \newline cc) (update-pos! r))
                  (recur))))
            (recur (get-char! r)))
          c)))))

(defn skip-to
  "Skip over the whitespace to the required character. If there are nonwhitespace characters, this is an error.
  r - The reader to parse from.
  c - The last character read.
  end-chars? - the set of chars to skip to.
  return: c
  c - the first character after the requested ending characters"
  [r c end-chars?]
  (loop [c c]
    (cond
      (end-chars? c) (get-char! r)
      (whitespace? c) (do
                        (when (= \newline c) (update-pos! r))
                        (recur (get-char! r)))
      (= \# c) (do
                 (loop [cc (get-char! r)]
                   (if (end-comment? cc)
                     (when (= \newline cc) (update-pos! r))
                     (recur (get-char! r))))
                 (recur (get-char! r)))
      :default (throw-unex *loc* "Unexpected character: " r (str c)))))

(defn skip-past-dot
  "Skip to the terminating dot. If non-whitespace is found, then report an error.
  r - The reader to parse from.
  return: c"
  [r c]
  (let [c (skip-whitespace r c)]
    (if (= \. c)
      (get-char! r)
      (let [p (position r)]
        (throw (ex-info (str "Unexpected character found at offset: " p) {:offset p :character c}))))))

(defn parse-prefix
  "Parse a prefix. This is a simple string terminated with a ':'. The : character is not part of the prefix.
  s - The reader to parse from.
  c - The last character read.
  return: [c prefix]
  c - The character immediately after the prefix.
  prefix - The prefix string."
  [r c]
  (if (= \: c)
    [(get-char! r) ""]
    (let [sb (text/string-builder)]
      (cond
        (pn-chars-base? c) (text/append! sb c)
        
        (text/high-surrogate? c) (let [c2 (get-char! r)]
                                   (when-not (text/low-surrogate? c2)
                                     (throw-unex *loc* "Bad Unicode characters at start of prefix: " r (str [c c2])))
                                   (text/append! sb c)
                                   (text/append! sb c2))
        :default (throw-unex *loc* "Unexpected character at start of prefix: " r (str \' c \')))
      (loop [c (get-char! r)]
        (cond
          (= \: c) (if (= \. (text/last-char sb))
                     (throw-unex *loc* "Unexpected '.' at end of prefix: " r (str sb))
                     [(get-char! r) (str sb)])
          (pn-chars? c) (do
                          (text/append! sb c)
                          (recur (get-char! r)))
          (text/high-surrogate? c) (let [c2 (get-char! r)]
                                     (when-not (text/low-surrogate? c2)
                                       (throw-unex *loc* (str "Bad Unicode characters " [c c2] " in prefix: ") r (str sb)))
                                     (text/append! sb c)
                                     (text/append! sb c2)
                                     (recur (get-char! r)))
          :default (throw-unex *loc* (str "Unexpected character '" c "' (" (text/char-code c) ") in prefix: ") r (str sb)))))))

(defn parse-u-char
  "Parse a an unescapped code of uxxxx or Uxxxxxxxx. A preceding \\ character was just parsed.
  r - the reader to parse from.
  f - the character found at position n. Must be u or U.
  return: [c char]
  c - the char immediately after the u-char sequence
  char - the character code that was parsed, which may be a char, or a string with that char"
  [r f]
  (case f
    \u (let [data (rdr/readn! r 4)
             end-char (get-char! r)]
         (if (= :eof end-char)
           (throw-unex *loc* "Unexpected end of file while parsing a unicode sequence:" r data)
           [end-char (char (text/parse-hex data))]))
    \U (let [data (rdr/readn! r 8)
             end-char (get-char! r)]
         (if (= :eof end-char)
           (throw-unex *loc* "Unexpected end of file while parsing a long unicode sequence:" r data)
           (let [unicode (text/parse-hex data)
                 [high low] (text/surrogates unicode)]
             [end-char (str (char high) (char low))])))
    (throw-unex *loc* "Unexpected non-U character when processing unicode escape:" r (str f))))

;; A regex to find the scheme at the start of an IRI
(def scheme-re #"^[A-Za-z][A-Za-z0-9.+-]*:")

(defn relative-iri?
  "Indicates if an IRI is relative."
  [s]
  (nil? (re-find scheme-re s)))

(defn parse-iri-ref
  "Parse an iri references. This is an iri string surrounded by <> characters. The <> characters are not returned.
  r - The reader to parse from.
  c - the first character of the iri reference.
  gen - the current generator
  triples - the current triples
  return: [c iri gen triples]
  c - the character at offset n.
  iri - The iri string.
  gen - the generator.
  triples - the current triples."
  [r c gen triples]
  (when-not (= c \<)
    (throw-unex *loc* "Unexpected character commencing an IRI Reference: " r (str c)))
  (let [sb (text/string-builder)]
    (loop [c (get-char! r)]
      (if (= c \>)
        (let [i (str sb)
              iri (new-iri gen (if (relative-iri? i)
                                 (if-let [base (get-base gen)] (str base i) i)
                                 i))]
          [(get-char! r) iri gen triples])
        (if (non-iri-char? c)
          (throw-unex *loc* "Unexpected character in IRI: " r (str sb))
          (if (= c \\)
            (if-let [[c' ch] (parse-u-char r (get-char! r))]
              (do
                (text/append! sb ch)
                (recur c'))
              (throw-unex *loc* "Unexpected \\ character in IRI: " r (str sb)))
            (do
              (text/append! sb c)
              (recur (get-char! r)))))))))

(defn parse-local
  "Parse a local into a string.
  r - The reader to parse from.
  return: [c local]
  c - the character after the local string.
  local - the parsed local name."
  [r]
  (let [sb (text/string-builder)
        add-char (fn [c] ;; adds a char, looking ahead if this is an escape sequence
                   (case c
                     \% (let [a (get-char! r)
                              b (get-char! r)]
                          (if (and (hex? a) (hex? b))
                            (do
                              (text/append! sb c)
                              (text/append! sb a)
                              (text/append! sb b))
                            (throw-unex *loc* "Bad escape code in localname: " r (str sb))))
                     \\ (let [a (get-char! r)]
                          (if (local-esc? a)
                            (text/append! sb a)
                            (throw-unex *loc* "Bad escape code in localname: " r (str sb))))
                     (text/append! sb c)))
        f (get-char! r)
        _ (when-not (local-chars? f)
            (throw-unex *loc* (str "Unexpected character '" f "' in local name: ") r (str sb)))
        n (add-char f)]
    (loop [c (get-char! r)]
      (if (= \. c) ;; at a dot. Check if this is inside a local name or terminating it
        (let [c' (get-char! r)]
          (if (local-chars2? c')
            ;; the next character is a valid local name character, so save and continue parsing
            (do
              (text/append! sb c) ;; a dot, so don't need to check for PLX (%hh or \ escape)
              (recur c'))
            ;; no, this must mean the local name ended already, and the dot is terminating a line
            ;; return the current name, along with the position of the dot
            [c (str sb)]))
        (if (local-chars2? c)
          ;; a valid local name char, so save and continue
          (do
            (add-char c)
            (recur (get-char! r)))
          [c (str sb)])))))

(defn parse-prefixed-name
  "Parse a prefix:local-name pair.
  r - The reader to parse from.
  pre - Optional. The initial part of the string that has already been parsed as a string builder.
  c - the first character of the prefixed name.
  gen - the generator
  triples - the current triples.
  return: [c prefix]
  c - The character immediately after the prefixed name.
  qname - The prefixed name as a IriRef
  gen - the updated generator.
  triples - the triples generated in parsing the node."
  ([r c gen triples]
   (when-not (or (pn-chars-base? c) (= \: c))
     (throw-unex *loc* "Prefix char starts with illegal character" r (str c)))
   (parse-prefixed-name r nil c gen triples))
  ([r pre c gen triples]
   (let [sb (or pre (text/string-builder))
         prefix (loop [c c dot false]
                  (if (= \: c)
                    (if dot
                      (throw-unex *loc* "Prefix illegally ends with a '.': " r (str sb))
                      (str sb))
                    (if (pn-chars-dot? c)
                      (do
                        (text/append! sb c)
                        (recur (get-char! r) (dot? c)))
                      (if (and (whitespace? c) (= "a" (str sb)))
                        nil
                        (throw-unex *loc* (str "Illegal character '" c "' in prefix: ") r (str sb))))))]
     (if prefix
       (let [[c local] (parse-local r)]
         [c (new-qname gen prefix local) gen triples])
       [(get-char! r) (rdf-type gen) gen triples]))))

(defn parse-iri
  "Parse an iri.
  r - the reader to parse from.
  c - the char found at position n.
  gen - the generator to use.
  triples - the current triples.
  return: [c iri]
  c - the character after the iri.
  iri - the node for the parsed iri. Either an IRI string or an IriRef.
  gen - the updated generator.
  triples - the triples generated in parsing the node."
  [r c gen triples]
  (if (= \< c)
    (parse-iri-ref r c gen triples)
    (parse-prefixed-name r c gen triples)))

(def echars "Maps ascii characters into their escape code"
  {\b \backspace
   \t \tab
   \n \newline
   \r \return
   \f \formfeed
   \\ \\})

(defn escape
  "Reads an escaped code from the input starting at the current position.
  r - the reader to parse from.
  c - the character after the \\ character.
  return: [c value]
  c - the character after the escape sequence
  value - the unescaped character."
  [r c]
  (if-let [e (echars c)]
    [(get-char! r) e]
    (if (#{\u \U} c)
      (parse-u-char r c)
      (throw-unex *loc* (str "Unexpected escape character <" c "> found in literal: ") r (str c)))))

(defn parse-long-string
  "Parse a triple-quoted string form. Because this is already identified as a triple quote
  the offset of n and the character represent the first character after the quotes.
  r - the reader to parse from, after the quotes.
  c - the first char in the string.
  end-q - the ending quote character to terminate on, either ' or \".
  return: [c value]
  c - the character following the closing quotes.
  value - the parsed string."
  [r c end-q]
  (let [sb (text/string-builder)]
    (loop [esc false current c]
      (cond
        (= end-q current)
        (if esc
          (do
            (text/append! sb current)
            (recur false (get-char! r)))
          (let [next-char (get-char! r)
                c2 (get-char! r)]
            (if (= end-q next-char) 
              (let [c3 (get-char! r)]
                (if (= end-q c2)
                  [c3 (str sb)]
                  (do    ;; third character was not a third quote
                    (text/append! sb current)
                    (text/append! sb next-char)
                    (if (= \\ c2)
                      (recur true c3)
                      (do
                        (when (= \newline c2) (update-pos! r))
                        (text/append! sb c2)
                        (recur false c3))))))
              (do  ;; second character was not a second quote
                (text/append! sb current)
                (if (= \\ next-char)
                  (recur true c2)
                  (do
                    (when (= \newline next-char) (update-pos! r))
                    (text/append! sb next-char)
                    (recur false c2)))))))

        (= :eof current)
        (throw-unex *loc* "Improperly terminated long literal: " r (str sb))

        :default
        (if esc
          (let [[c2 ecode] (escape r current)]
            (text/append! sb ecode)
            (recur false c2))
          (let [next-char (get-char! r)]
            (if (= \\ current)
              (recur true next-char)
              (do
                (text/append! sb current)
                (recur false next-char)))))))))

(defn parse-string
  "Parse a single-quoted string form.
  end-q - the ending quote character to terminate on.
  r - the reader to parse from.
  c - the last read char. This is after the opening quote character.
  return: [c value]
  c - the character immediately after the string.
  value - the parsed string. "
  [r c end-q]
  (let [sb (text/string-builder)]
    (loop [esc false current c]
      (cond
        (= end-q current) ;; end of the string, unless escaped
        (let [next-char (get-char! r)]
          (if esc
            (do
              (text/append! sb current)
              (recur false next-char))
            [next-char (str sb)]))

        (= :eof current)
        (throw-unex *loc* "Improperly terminated literal: " r (str sb))

        :default
        (if esc
          (let [[c2 ecode] (escape r current)]
            (text/append! sb ecode)
            (recur false c2))
          (let [next-char (get-char! r)]
            (if (= \\ current)
              (recur true next-char)
              (do
                (when (= \newline current) (update-pos! r))
                (text/append! sb current)
                (recur false next-char)))))))))

(defn parse-lang-tag
  "Parse the language tag from a literal. The @ character has already been parsed.
  r - the reader to parse from.
  c - the last char read.
  gen - the node generator.
  triples - the current triples.
  lit-str - the literal string that this tag affects.
  return: [c literal gen triples]
  c - the character after the lang tag.
  literal - the parsed value, as a language literal.
  gen - the updated generator.
  triples - the triples generated in parsing the node."
  [r c gen triples lit-str]
  (let [sb (text/string-builder)
        nc (loop [c' c]
             (if (= :eof c')
               c'
               (let [cn (int c')]
                 (cond
                   (and (<= cn (int \z)) (>= cn (int \A))
                        (or (>= cn (int \a)) (<= cn (int \Z))))
                   (do (text/append! sb c')
                       (recur (get-char! r)))

                   (= c' \-)
                   (if-not (text/buffer-empty? sb)
                     (do (text/append! sb c')
                         nil)
                     (throw-unex *loc* "Bad language tag on literal: " r (str sb)))
                        
                   :default
                   c'))))]
    (if nc
      [nc (new-lang-string gen lit-str (str sb)) gen triples]
      (loop [c' (get-char! r) group-start? true]
        (if (= :eof c')
          [c' (new-lang-string gen lit-str (str sb)) gen triples]
          (let [cn (int c')]
            (cond
              (and (<= cn (int \z)) (>= cn (int \0))
                   (or (>= cn (int \a))
                       (and (<= cn (int \Z)) (>= cn (int \A)))
                       (<= cn (int \9))))
              (do (text/append! sb c')
                  (recur (get-char! r) false))

              (= c' \-)
              (if group-start?
                (throw-unex *loc* "Bad language tag on literal: " r (str sb))
                (do (text/append! sb c')
                    (recur (get-char! r) true)))
              
              :default
              (if group-start?
                (throw-unex *loc* "Bad language tag on literal: " r (str sb))
                [c' (new-lang-string gen lit-str (str sb)) gen triples]))))))))

(defn parse-literal
  "Parse a literal that starts with a quote character. This also includes the
  triple quote form that allows for raw unescaped strings.
  r - the reader to parse from.
  c - the last char read. This is a quote: either ' or \"
  gen - the node generator.
  triples - the current triples.
  return: [c value gen triples]
  c - the first character after the literal.
  value - the parsed value, in string form if it is plain.
  gen - the updated generator.
  triples - the triples generated in parsing the node."
  [r quote-c gen triples]
  (let [c' (get-char! r)
        [c lit-str] (if (= c' quote-c)
                      (let [c2 (get-char! r)]
                        (if (= c2 quote-c)
                          (parse-long-string r (get-char! r) quote-c)
                          [c2 ""]))
                      (parse-string r c' quote-c))]
    (case c
      \^ (let [tc (get-char! r)]
           (if (= \^ tc)
             (let [[c' iri gen triples] (parse-iri r (get-char! r) gen triples)]
               [c' (new-literal gen lit-str iri) gen triples])
             (throw-unex *loc* "Badly formed type expression on literal. Expected ^^: " r (str tc))))
      \@ (parse-lang-tag r (get-char! r) gen triples lit-str)
      [c lit-str gen triples])))

(defn parse-number
  "Parse a numeric literal.
  r - the reader to parse from.
  c - the char found at position n.
  gen - the node generator.
  triples - the current triples.
  return: [c value]
  c - the character immediately after the number.
  value - the parsed number.
  gen - the updated generator
  triples - the triples generated in parsing the node."
  [r c gen triples]
  (let [sb (text/string-builder)
        ;; determine the sign
        c (case c
            \+ (get-char! r)
            \- (do
                 (text/append! sb c)
                 (get-char! r))
            c)
        ;; get all the digits, and up to 1 dot. If a dot exists, then dbl? will be true
        [c dbl? digits?] (loop [lc c d? false dg? false]
                           (case lc
                             (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9) (do
                                                               (text/append! sb lc)
                                                               (recur (get-char! r) d? true))
                             \. (if d?
                                  [lc d? dg?]
                                  (do
                                    (text/append! sb lc)
                                    (recur (get-char! r) true dg?)))
                             [lc d? dg?]))
        _ (when-not digits?
            (throw-unex *loc* (str "Invalid floating point number in:") r (str sb)))
        ;; read an exponent trailer, if one exists
        [c dbl?] (if (or (= c \e) (= c \E))
                   (let [_ (text/append! sb c)
                         c' (get-char! r)
                         ;; read the optional exponent sign
                         c' (if (or (= c' \+) (= c' \-))
                              (do
                                (text/append! sb c')
                                (get-char! r))
                              c')]
                     ;; read the exponent digits
                     (loop [cc c' dgts? false]
                       (case cc
                         (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9) (do
                                                           (text/append! sb cc)
                                                           (recur (get-char! r) true))
                         (if dgts?
                           [cc true]
                           (throw-unex *loc* (str "Invalid floating point number in:") r (str sb))))))
                   ;; no exponent
                   [c dbl?])
        nr (if dbl?
             (parse-double (str sb))
             (parse-long (str sb)))]
    [c nr gen triples]))

(declare parse-predicate parse-object)

(defn parse-collection
  "Parses a collection. This creates a linked list in the triples.
  r - The parser to parse from.
  c - the first character of the collection
  gen - the current generator
  triples - the current triples
  return: [c node gen triples]
  c - the character immediately after the collection.
  node - The node representing the collection.
  gen - the generator.
  triples - the current triples."
  [r c gen triples]
  (let [c (skip-whitespace r (get-char! r))
        rfirst (rdf-first gen)
        rrest (rdf-rest gen)
        rnil (rdf-nil gen)]
    (if (= \) c)
      [(get-char! r) rnil gen triples]
      (let [[gen head] (new-node gen)]
        (loop [last-node head [c node gen triples] (parse-object r c gen triples)]
          (let [triples (triples/append! triples last-node rfirst node)
                c (skip-whitespace r c)]
            (if (= \) c)
              (let [triples (triples/append! triples last-node rrest rnil)]
                [(get-char! r) head gen triples])
              (let [[gen node] (new-node gen)
                    triples (triples/append! triples last-node rrest node)]
                (recur node (parse-object r c gen triples))))))))))

(defn parse-blank-node
  "Parses a blank node label.
  r - The reader to parse from.
  n - The offset to parse from.
  c - the first character of the blank node
  gen - the current generator
  triples - the current triples
  return: [c node gen triples]
  n - The offset immediately after the prefix.
  c - the character at offset n.
  node - The blank node.
  gen - the generator.
  triples - the current triples."
  [r c gen triples]
  (when-not (= \: (get-char! r))
    (throw-unex *loc* "Illegal underscore (_) at start of symbol." r (str \_ (rdr/readn-line! r))))
  (let [c (get-char! r)
        _ (when-not (pn-chars-ud? c)
            (throw-unex *loc* "Illegal character at start of blank node label: '" r (str c (rdr/readn-line! r) "'")))
        sb (text/string-builder)
        _ (text/append! sb c)
        [c label] (loop [c (get-char! r) dot false]
                    (if (pn-chars-dot? c)
                      (do
                        (text/append! sb c)
                        (recur (get-char! r) (dot? c)))
                      (if dot
                        (throw-unex *loc* "blank node illegally ends with a '.': " r (str sb))
                        [c (str sb)])))
        [gen node] (new-node gen label)]
    [c node gen triples]))

(def end-list? #{\] \.})

(defn maybe-parse-predicate
  "Parses an IRI as a predicate if one is available. Otherwise return a nil as the IRI.
  r - The reader to parse from.
  c - the first character of the iri reference.
  gen - the current generator
  triples - the current triples
  return: [c iri gen triples]
  c - the last character read.
  iri - The iri string.
  gen - the generator.
  triples - the current triples."
  [r c gen triples]
  (if (end-list? c)
    [c nil gen triples]
    (parse-iri r c gen triples)))

(defn parse-predicate-object-list
  "Parse a predicate-object list
  r - The reader to parse from
  c - The character at position n
  gen - The generator for blank nodes and namespace resolution
  triples - An accumulating transient of triples.
  return [c gen triples]
  c - last character read
  gen - the updated generator
  triples - the updated triples sequence."
  [r c subject gen triples]
  (loop [[c pred gen triples] (maybe-parse-predicate r c gen triples)]
    (if-not pred
      [c gen triples]
      (let [c (skip-whitespace r c)
            [c gen triples] (loop [[c obj gen triples] (parse-object r c gen triples)]
                              (let [c (skip-whitespace r c)
                                    triples (triples/append! triples subject pred obj)]
                                (case c
                                  (\] \. \;) [c gen triples]
                                  \, (let [c (skip-whitespace r (get-char! r))]
                                       (if-let [parse-result (try
                                                               (parse-object r c gen triples)
                                                               (catch ExceptionInfo e nil))]
                                         (recur parse-result)
                                         [c gen triples]))
                                  (throw-unex *loc* "Unexpected separator in predicate-object list: '" r (str c "' " (last (seq triples)))))))]
        (if (= c \;)
          (let [c (skip-whitespace r (get-char! r))]
            (recur (maybe-parse-predicate r c gen triples)))
          [c gen triples])))))

(defn anon-blank-node
  "Generates a new blank node with no properties. Already passed the opening [ character, and whitespace.
  This function just steps to the next position.
  r - The reader to parse from.
  c - The last character read. This must be a ]
  g - The generator.
  triples - An accumulating transient of triples.
  return: [r node gen triples more?]
  c - the character immediately after the node
  node - the new anonymous blank node
  gen - the updated generator
  triples - the updated triples sequence
  enf? - If this is a subject, then is this is not enough and properties are required. Always false."
  [r c g triples]
  (let [[g node] (new-node g)]
    [(get-char! r) node g triples false]))

(defn parse-blank-node-entity
  "Parse a blank node property/value list. Already past the opening [ character and whitespace.
  s - The reader to parse from
  c - The last character read
  gen - The generator for blank nodes and namespace resolution
  triples - An accumulating transient of triples.
  return [c node gen triples]
  c - The last character read
  node - the root blank node that is being parsed
  gen - the updated generator
  triples - the updated triples sequence
  enf? - is this enough if this is a subject? A predicateObject sequence is not needed. true."
  [r c gen triples]
  (let [[gen node] (new-node gen)
        [c gen triples] (parse-predicate-object-list r c node gen triples)
        c (skip-whitespace r c)]
    (if (= c \])
      [(get-char! r) node gen triples true]
      (throw-unex *loc* "Structured blank node entity improperly terminated with '" r (str c "' " (last (seq triples)))))))

(defn parse-subject
  "Parse a subject entity, including any triples.
  r - the reader to parse from.
  c - the char found at position n.
  gen - the node generator.
  triples - the current triples.
  return: [c subject gen triples]
  c - The last character read
  subject - the node for the parsed subject.
  gen - the updated generator
  triples - the triples generated in parsing the node.
  enf? - indicates if this subject is enough and a predicateObject list is not needed.
         Most types return nil for this (falsey)."
  [r c gen triples]
  (case c
    \< (parse-iri-ref r c gen triples)
    \( (parse-collection r c gen triples)
    \_ (parse-blank-node r c gen triples)
    \[ (let [c (skip-whitespace r (get-char! r))]
         (if (= \] c)
           (anon-blank-node r c gen triples)
           (parse-blank-node-entity r c gen triples)))
    (parse-prefixed-name r c gen triples)))

(defn parse-ambiguous-elt
  "Reads an object to the point where ambiguity can be resolved.
  Processes text as either a prefixed name or the text being searched on.
  r - The reader to parse from.
  c - The last character read
  gen - The current generator.
  triples - The current triples.
  common-text - The text that can start either type of line.
  non-iri-op - The function to use to parse the line if it does not contain triples.
                   This function accepts the string and offset into the string
  return: [c gen triples]
  c - The last character read
  gen - the next generator state.
  obj - the object generated from parsing this object."
  [r c gen triples common-text non-iri-op]
  (let [common-len (count common-text)
        text (text/string-builder (subs common-text 0 1))]
    (loop [a 1 cp (get-char! r)]
      (if (and (< a common-len) (= (text/char-at common-text a) (text/lower-case-char cp)))
        (do
          (text/append! text cp)
          (recur (inc a) (get-char! r)))
        (if (or (pn-chars? cp) (= \: cp))
          (parse-prefixed-name r text cp gen triples)
          (non-iri-op))))))

(defn parse-object
  "Parse an object entity, including any triples.
  r - the reader to parse from.
  c - the char found at position n.
  gen - the node generator.
  triples - the current triples.
  return: [c object gen triples]
  c - The last character read
  object - the node for the parsed object.
  gen - the updated generator.
  triples - the triples generated in parsing the node.
  Blank node entities can also return a true at the end of the vector, but this should be ignored."
  [r c gen triples]
  (case c
    \< (parse-iri-ref r c gen triples)
    \( (parse-collection r c gen triples)
    \_ (parse-blank-node r c gen triples)
    \[ (let [c (skip-whitespace r (get-char! r))]
         (if (= \] c)
           (anon-blank-node r c gen triples)
           (parse-blank-node-entity r c gen triples)))
    (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \. \+ \-) (parse-number r c gen triples)
    (\' \") (parse-literal r c gen triples)
    \f (parse-ambiguous-elt r c gen triples "false"
                            (fn [] [(get-char! r) false gen triples]))
    \t (parse-ambiguous-elt r c gen triples "true"
                            (fn [] [(get-char! r) true gen triples]))
    (parse-prefixed-name r c gen triples)))

(defn parse-triples
  "Parse a top level triples from a string.
  r - the reader to parse from.
  c - the character found at position n.
  gen - the generator to use for blank nodes.
  triples - A transient vector of triples.
  return: [c gen triples]
  c - The last character read
  gen - the next generator state.
  triples - the triples generated from parsing this line."
  [r c gen triples]
  (let [initial-count (count triples)
        c (skip-whitespace r c)]
    (if (= :eof c)
      [:eof gen triples]
      (let [[c subject gen triples enf?] (parse-subject r c gen triples)
            c (skip-whitespace r c)
            [c gen triples] (parse-predicate-object-list r c subject gen triples)]
        (when-not (= \. c)
          (throw-unex *loc* "Statements invalidly terminated: " r (str (last (seq triples)) \' c \')))
        (when (and (not enf?) (= initial-count (count triples)))
          (throw-unex *loc* "Subjects require predicates and objects: " r c))
        [(get-char! r) gen triples]))))

(defn pre-parse-triples
  "Parse a top level triples from a string,
  with some of the first triple already parsed as a prefixed stirng.
  r - the reader to parse from.
  c - the character found at position n.
  gen - the generator to use for blank nodes.
  triples - A transient vector of triples.
  pre - the previously read prefix of the line in a string builder
  return: [c gen triples]
  c - The last character read
  gen - the next generator state.
  triples - the triples generated from parsing this line."
  [r c gen triples pre]
  (let [initial-count (count triples)]
    (if (= :eof c)
      (throw-unex *loc* "Unexpected termination while parsing a prefixed line: " r c)
      (let [[c subject gen triples enf?] (parse-prefixed-name r pre c gen triples)
            c (skip-whitespace r c)
            [c gen triples] (parse-predicate-object-list r c subject gen triples)]
        (when-not (= \. c)
          (throw-unex *loc* "Statements invalidly terminated: " r (str (last (seq triples)) c)))
        (when (and (not enf?) (= initial-count (count triples)))
          (throw-unex *loc* "Subjects require predicates and objects: " r c))
        [(get-char! r) gen triples]))))

(defn parse-prefix-iri-end
  "Parse an iri and a newline for a PREFIX or BASE directive.
  NOTE: THE FIRST ARITY OF THIS FUNCTION DOES NOT USE AN INCOMING CHARACTER
  r - The reader to parse from.
  c - [optional] The incoming character.
  gen - The generator to update.
  end-char - A test for the final character
  return: [c gen]
  c - character at position n
  gen - the new generator."
  ([r gen end-char]
   (let [c (get-char! r)]
     (if (whitespace? c)
       (parse-prefix-iri-end r c gen end-char)
       (throw-unex *loc* "Unknown statement: " r (str \' c \')))))
  ([r c gen end-char]
   (let [c (skip-whitespace r c)
         [c prefix] (parse-prefix r c)
         c (skip-whitespace r c)
         [c iri] (parse-iri-ref r c gen nil)
         c (skip-to r c end-char)]
     [c (add-prefix gen prefix iri)])))

(defn parse-base-end
  "Parse an iri and the end-char for the end of the line.
  NOTE: THE FIRST ARITY OF THIS FUNCTION DOES NOT USE AN INCOMING CHARACTER
  r - The reader to parse from.
  c - [optional] The incoming character.
  gen - The generator to update.
  end-char - A test for the final character
  return: [c gen]
  c - character at position n
  gen - the new generator."
  ([r gen end-char]
   (let [c (get-char! r)]
     (if (whitespace? c)
       (parse-base-end r c gen end-char)
       (throw-unex *loc* "Unknown statement: " r (str \' c \')))))
  ([r c gen end-char]
   (let [c (skip-whitespace r c)
         ;; nil triples, since triples are not being generated during a directive
         [c iri] (parse-iri-ref r c gen nil)
         c (skip-to r c end-char)]
     [c (add-base gen iri)])))

(defn parse-ambiguous-line
  "Reads a line up to the point where ambiguity can be resolved.
  Processes the line as either the beginning of triples, or a provided operation.
  r - The reader to parse from.
  c - The last character read. (Optional: can be inferred)
  gen - The current generator.
  triples - The current triples.
  common-text - The text that can start either type of line.
  non-triples-op - The function to use to parse the line if it does not contain triples.
                   This function accepts the incoming char.
  return: [c gen triples]
  c - The last character read.
  gen - the next generator state.
  triples - the triples generated from parsing this line."
  [r c gen triples common-text non-triples-op]
  (let [common-len (count common-text)
        text (text/string-builder)]
    (loop [a 0 cp c]
      (if (and (< a common-len) (= (text/char-at common-text a) (text/lower-case-char cp)))
        (do
          (text/append! text cp)
          (recur (inc a) (get-char! r)))
        (if (whitespace? cp) ;; should only occur when the common length was reached
          (non-triples-op cp)
          (pre-parse-triples r cp gen triples text))))))

(defn parse-statement
  "Parse a directive or triples.
  r - The reader to parse from.
  c - The last character read. (Optional: can be inferred)
  gen - The current generator.
  triples - The current triples.
  return: [c gen triples]
  c - The last character read.
  gen - the next generator state.
  triples - the triples generated from parsing this line."
  ([r gen triples] (parse-statement r (get-char! r) gen triples))
  ([r c gen triples]
   (let [c (skip-whitespace r c)
         [c' gen' triples'] (case c
                              \@ (let [text (rdr/readn! r 4)]
                                   (if (= text "base")
                                     (parse-base-end r gen dot?)
                                     (let [text2 (str text (rdr/readn! r 2))]
                                       (if (= text2 "prefix")
                                         (parse-prefix-iri-end r gen dot?)
                                         (throw-unex *loc* "Unknown statement: " r text2)))))
                              (\B \b) (parse-ambiguous-line r c gen triples "base"
                                                            #(parse-base-end r % gen newline?))
                              (\P \p) (parse-ambiguous-line r c gen triples "prefix"
                                                            #(parse-prefix-iri-end r % gen newline?))
                              :eof [c gen triples]
                              nil)]
     (if c'
       [c' gen' (or triples' triples)]
       (parse-triples r c gen triples)))))

(defn parse
  "parse a string as a turtle document
  in - the string or Reader containing the document.
  g - an implementation of the Generator protocol for assigning blank nodes, IRIs and Literals,
      and managing namespaces. Optional.
  return: {:base <optional IRI>
           :namespaces <prefixes mapped to IRIs>
           :triples <vector of 3 element vectors>}"
  ([in] (parse in (new-generator)))
  ([in generator]
   (reset-pos!)
   (let [triples (triples/triple-accumulator)
         reader (rdr/position-reader in)
         [gen triples] (binding [*loc* (volatile! [1 0])]
                           (loop [[c gen triples] (parse-statement reader generator triples)]
                             (if (= :eof c)
                               [gen triples]
                               (recur (parse-statement reader c gen triples)))))
         result {:namespaces (get-namespaces gen)
                 :triples (seq triples)}
         base (get-base gen)]
     (if base
       (assoc result :base base)
       result))))
