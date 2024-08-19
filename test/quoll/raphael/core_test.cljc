(ns quoll.raphael.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [quoll.raphael.core :as core
             :refer [skip-whitespace skip-to dot? newline? add-base
                     parse-iri-ref add-prefix new-generator parse-statement
                     parse-local parse-prefixed-name parse-number parse-string
                     parse-long-string parse-literal parse-lang-tag
                     anon-blank-node parse-blank-node-entity parse-blank-node
                     new-lang-string new-literal new-iri
                     new-qname
                     parse-predicate-object-list parse-collection
                     parse]]
            [quoll.raphael.reader :as rdr :refer [position-reader get-char!]]
            [quoll.raphael.text :refer [char-at]]
            [quoll.raphael.triples :refer [triple-accumulator]]
            [quoll.rdf :as rdf :refer [iri unsafe-blank-node RDF-NIL RDF-FIRST RDF-REST RDF-TYPE typed-literal XSD-STRING]]
            [tiara.data :refer [EMPTY_MAP]])
  #?(:clj (:import [clojure.lang ExceptionInfo])))

#?(:clj (set! *warn-on-reflection* true))

(defn blank-node [n] (unsafe-blank-node (str "b" n)))

(defn skip-whitespace' [s] (let [r (position-reader s)]
                             (skip-whitespace r (get-char! r))))

(deftest ws-test
  (testing "Tests if whitespace is skipped correctly."
    (is (= \a (skip-whitespace' "   a b")))
    (is (= \b (skip-whitespace' "     b")))
    (is (= \b (skip-whitespace' "   # this is whitespace\n  b")))
    (is (= :eof (skip-whitespace' "")))
    (is (= :eof (skip-whitespace' "      ")))))

(defn skip-to' [s chars] (let [r (position-reader s)]
                           (skip-to r (get-char! r) chars)))

(deftest skip-to-test
  (testing "Testing skipping whitespace to a character"
    (is (= \space (skip-to' "   . a" dot?)))
    (is (thrown? ExceptionInfo (skip-to' "    a" dot?)))
    (is (= \space (skip-to' "   \n a" newline?)))
    (is (thrown? ExceptionInfo (skip-to' "      a" newline?)))))

(defn parse-iri-ref' [s c gen]
  (let [r (position-reader s)]
    (parse-iri-ref r c gen nil)))

(deftest iri-ref-test
  (testing "Parsing an IRI reference"
    (let [g (-> (new-generator) (add-base "http://test.org/"))
          i (fn [s] (new-iri g s))]
      (is (= [:eof (i "http://ex.com/") g nil]
             (parse-iri-ref' "http://ex.com/>" \< g)))
      (is (= [:eof (i "http://example.com/path?query=x&y=2#fragment") g nil]
             (parse-iri-ref' "http://example.com/path?query=x&y=2#fragment>" \< g)))
      (is (= [:eof (i "http://ex.com/") g nil]
             (parse-iri-ref' "http://ex.com/>" \< g)))
      (is (= [:eof (i "http://example.com/path?query=x&y=2#fragment") g nil]
             (parse-iri-ref' "http://example.com/path?query=x&y=2#fragment>" \< g)))
      (is (thrown? ExceptionInfo
                   (parse-iri-ref' "http://example.com/path query=x&y=2#fragment>" \< g)))
      (is (thrown? ExceptionInfo
                   (parse-iri-ref' "http://example.com/path?query=x&y=2#fragment>" \h g)))
      (is (thrown? ExceptionInfo
                   (parse-iri-ref' "http://example.com/path?query=x&y=2#fragment" \< g)))
      (let [ge (new-generator)]
        (is (= [:eof (new-iri ge "path") ge nil] (parse-iri-ref' "path>" \< ge))))
      (is (= [:eof (i "http://test.org/path") g nil] (parse-iri-ref' "path>" \< g))))))

(defn parse-statement' [s gen] (parse-statement (position-reader s) gen nil))
(defn parse-statement-t [s gen]
  (let [r (position-reader s)
        [c g t] (parse-statement r (get-char! r) gen (triple-accumulator))]
    [c g (seq t)]))

(deftest non-triple-statement-test
  (testing "parsing statements that are not triples"
    (let [g (new-generator)
          [c1 g1 t1] (parse-statement' "BASE <http://test.org/>\n" g)
          [c2 g2 t2] (parse-statement' "@base <http://test.com/>. \n" g1)
          [c3 g3 t3] (parse-statement' "PREFIX test: <http://test.org/>\n" g2)
          [c4 g4 t4] (parse-statement' "@prefix t2: <http://test.com/>. \n" g3)
          [c5 g5 t5] (parse-statement' "@prefix t\ud800\udd49: <http://atticten.com/>. \n" g4)
          [c6 g6 t6] (parse-statement' "@prefix \ud800\udd49x: <http://atticten.com/pre>. \n" g5)
          [c7 g7 t7] (parse-statement' "@prefix \udb7f\udfffx: <http://unicode.com/limit>. \n" g6)
          [c8 g8 t8] (parse-statement' "@prefix : <http://short.com/>. \n" g7)
          [c9 g9 t9] (parse-statement' "PREFIX test: <http://test.org#>\n" g2)]
      (is (= :eof c1))
      (is (= {:base "http://test.org/"} (:namespaces g1)))
      (is (nil? t1))
      (is (= \space c2))
      (is (= {:base "http://test.com/"} (:namespaces g2)))
      (is (nil? t2))
      (is (= :eof c3))
      (is (= {:base "http://test.com/" "test" "http://test.org/"} (:namespaces g3)))
      (is (nil? t3))
      (is (= \space c4))
      (is (= {:base "http://test.com/" "test" "http://test.org/" "t2" "http://test.com/"} (:namespaces g4)))
      (is (nil? t4))

      (is (= \space c5))
      (is (= {:base "http://test.com/" "test" "http://test.org/" "t2" "http://test.com/"
              "t\ud800\udd49" "http://atticten.com/"}
             (:namespaces g5)))
      (is (nil? t5))
      (is (= \space c6))
      (is (= {:base "http://test.com/" "test" "http://test.org/" "t2" "http://test.com/"
              "t\ud800\udd49" "http://atticten.com/" "\ud800\udd49x" "http://atticten.com/pre"}
             (:namespaces g6)))
      (is (nil? t6))
      (is (= \space c7))
      (is (= {:base "http://test.com/" "test" "http://test.org/" "t2" "http://test.com/"
              "t\ud800\udd49" "http://atticten.com/" "\ud800\udd49x" "http://atticten.com/pre"
              "\udb7f\udfffx" "http://unicode.com/limit"}
             (:namespaces g7)))
      (is (nil? t7))
      (is (= \space c8))
      (is (= {:base "http://test.com/" "test" "http://test.org/" "t2" "http://test.com/"
              "t\ud800\udd49" "http://atticten.com/" "\ud800\udd49x" "http://atticten.com/pre"
              "\udb7f\udfffx" "http://unicode.com/limit" "" "http://short.com/"}
             (:namespaces g8)))
      (is (nil? t8))
      (is (= :eof c9))
      (is (= {:base "http://test.com/" "test" "http://test.org#"} (:namespaces g9)))
      (is (nil? t9))


      (is (thrown? ExceptionInfo
                   (parse-statement' "@prefix \ud800\ud849x: <http://atticten.com/pre>. \n" g5)))
      (is (thrown? ExceptionInfo
                   (parse-statement' "@prefix \ud800x: <http://atticten.com/pre>. \n" g5)))
      (is (thrown? ExceptionInfo
                   (parse-statement' "@prefix \udd00\udd49x: <http://atticten.com/pre>. \n" g5)))
      (is (thrown? ExceptionInfo
                   (parse-statement' "@prefix \udb80\udc00x: <http://atticten.com/pre>. \n" g5))))))

(deftest triple-statement-test
  (testing "parsing statements that are not triples"
    (let [g (new-generator)
          [c1 g1 t1] (parse-statement-t "BASE <http://test.org/>\n" g)
          [c2 g2 t2] (parse-statement-t "PREFIX a: <http://a.org/>\n" g1)
          [c3 g3 t3] (parse-statement-t "a:a a:b 'test' ." g2)
          [c4 g4 t4] (parse-statement-t "a:a <b> (1 2) . " g3)
          [c5 g5 t5] (parse-statement-t "[a:a 1; a:b 2] a a:X . " g3)
          [c6 g6 t6] (parse-statement-t "[a:a 1;a:b 2;a a:X].[" g3)]
      (is (= c3 :eof))
      (is (= (:namespaces g3) {:base "http://test.org/" "a" "http://a.org/"}))
      (is (= t3 [[(new-qname g3 "a" "a") (new-qname g3 "a" "b") "test"]]))
      (is (= c4 \space))
      (is (= t4 [[(blank-node 0) RDF-FIRST 1]
                 [(blank-node 0) RDF-REST (blank-node 1)]
                 [(blank-node 1) RDF-FIRST 2]
                 [(blank-node 1) RDF-REST RDF-NIL]
                 [(new-qname g3 "a" "a") (new-iri g3 "http://test.org/b") (blank-node 0)]]))
      (is (= c5 \space))
      (is (= t5 [[(blank-node 0) (new-qname g3 "a" "a") 1]
                 [(blank-node 0) (new-qname g3 "a" "b") 2]
                 [(blank-node 0) RDF-TYPE (new-qname g3 "a" "X")]]))
      (is (= c6 \[))
      (is (= t6 [[(blank-node 0) (new-qname g3 "a" "a") 1]
                 [(blank-node 0) (new-qname g3 "a" "b") 2]
                 [(blank-node 0) RDF-TYPE (new-qname g3 "a" "X")]])))))

(deftest parse-comment-test
  (testing "parsing statements that have comments"
    (let [g (new-generator)
          [c1 g1 t1] (parse-statement' "@base <http://test.org/> # comment\n. \n" g)
          [c2 g2 t2] (parse-statement' "@prefix a: #comment\n  <http://test.com/>. \n" g1)
          [c3 g3 t3] (parse-statement' "#comment\n" g2)
          [c4 g4 t4] (parse-statement-t "a:a #comment\na:b            #comment\n 'test'#comment\n." g2)]
      (is (= c1 \space))
      (is (= (:namespaces g1) {:base "http://test.org/"}))
      (is (= c2 \space))
      (is (= (:namespaces g2) {:base "http://test.org/" "a" "http://test.com/"}))
      (is (= c3 :eof))
      (is (nil? t3))
      (is (= c4 :eof))
      (is (= t4 [[(new-qname g2 "a" "a") (new-qname g2 "a" "b") "test"]])))))

(defn parse-local' [s] (parse-local (position-reader s)))

(deftest local-name-test
  (testing "Parsing the local portion of a prefixed name"
    (is (= [\space "alice"] (parse-local' "alice ")))
    (is (= [\. "alice"] (parse-local' "alice. ")))
    (is (= [\space "alice.b"] (parse-local' "alice.b ")))
    (is (= [\. "alice"] (parse-local' "alice. ")))
    (is (= [\space "alice%20b"] (parse-local' "alice%20b ")))
    (is (= [\space "alice!b"] (parse-local' "alice\\!b ")))
    (is (= [\space "alice%ba"] (parse-local' "alice\\%ba ")))
    (is (thrown? ExceptionInfo (parse-local' "alice%%b ")))
    (is (thrown? ExceptionInfo (parse-local' "alice%bg ")))
    (is (thrown? ExceptionInfo (parse-local' "alice\\bg ")))))

(defn parse-prefixed-name' [s gen] (parse-prefixed-name (position-reader (subs s 1)) (first s) gen nil))

(deftest prefixed-name-test
  (testing "Parsing prefixed names"
    (let [g (-> (new-generator)
                (add-prefix "ex" "http://ex.com/")
                (add-prefix "" "http://test.org/")
                (add-prefix "a-b-c" "http://t1.org/")
                (add-prefix "a_b" "http://t2.org/")
                (add-prefix "a.b" "http://t3.org/"))]
      (is (= [\space (iri "http://ex.com/cat" "ex" "cat") g nil]
             (parse-prefixed-name' "ex:cat " g)))
      (is (= [\. (iri "http://ex.com/cat" "ex" "cat") g nil]
             (parse-prefixed-name' "ex:cat. " g)))
      (is (= [\space (iri "http://test.org/cat" "" "cat") g nil]
             (parse-prefixed-name' ":cat " g)))
      (is (= [\. (iri "http://test.org/cat" "" "cat") g nil]
             (parse-prefixed-name' ":cat. " g)))
      (is (= [\space (iri "http://t1.org/cat" "a-b-c" "cat") g nil]
             (parse-prefixed-name' "a-b-c:cat " g)))
      (is (= [\space (iri "http://t2.org/cat" "a_b" "cat") g nil]
             (parse-prefixed-name' "a_b:cat " g)))
      (is (= [\space (iri "http://t3.org/cat" "a.b" "cat") g nil]
             (parse-prefixed-name' "a.b:cat " g)))
      (is (thrown? ExceptionInfo (parse-prefixed-name' "_b:cat " g)))
      (is (thrown? ExceptionInfo (parse-prefixed-name' ".b:cat " g)))
      (is (thrown? ExceptionInfo (parse-prefixed-name' "-b:cat " g))))))

(defn parse-number' [s] (parse-number (position-reader (subs s 1)) (first s) nil nil))

(deftest number-test
  (testing "Parsing numeric literals"
    (is (= [:eof 123 nil nil nil] (parse-number' "123")))
    (is (= [\space 123 nil nil nil] (parse-number' "123 ")))
    (is (= [\space 0 nil nil nil] (parse-number' "0 ")))
    (is (= [\. 0 nil nil :eof] (parse-number' "0. ")))
    (is (= [\. 0.0 nil nil \.] (parse-number' "0. .")))
    (is (= [\space 0.234 nil nil nil] (parse-number' "0.234 ")))
    (is (= [\space 0.234 nil nil nil] (parse-number' ".234 ")))
    (is (= [\space 23.4 nil nil nil] (parse-number' "23.40 ")))
    (is (= [\. 234 nil nil :eof] (parse-number' "234. ")))
    (is (= [\; 234.0 nil nil \;] (parse-number' "234. ;")))
    (is (= [:eof -123 nil nil nil] (parse-number' "-123")))
    (is (= [\space 123 nil nil nil] (parse-number' "+123 ")))
    (is (= [\space 0 nil nil nil] (parse-number' "+0 ")))
    (is (= [\. 0 nil nil :eof] (parse-number' "-0. ")))
    (is (= [\. 0.0 nil nil \.] (parse-number' "-0..")))
    (is (= [\space 0.234 nil nil nil] (parse-number' "+0.234 ")))
    (is (= [\space -0.234 nil nil nil] (parse-number' "-.234 ")))
    (is (= [\space 23.4 nil nil nil] (parse-number' "+23.40 ")))
    (is (= [\. -234 nil nil :eof] (parse-number' "-234. ")))
    (is (= [\; -234.0 nil nil \;] (parse-number' "-234.;")))
    (is (= [\space 23.4e2 nil nil nil] (parse-number' "23.4e2 ")))
    (is (= [\space 234.0e2 nil nil nil] (parse-number' "234.e2 ")))
    (is (= [\space -234.0e2 nil nil nil] (parse-number' "-234.e2 ")))
    (is (= [\space -23.4e2 nil nil nil] (parse-number' "-23.4e2 ")))
    (is (= [\space 0.234e12 nil nil nil] (parse-number' "+.234e12 ")))
    (is (= [\space 0.234e12 nil nil nil] (parse-number' ".234e12 ")))
    (is (= [\space 0.234e-12 nil nil nil] (parse-number' "+.234e-12 ")))
    (is (= [\space 0.234e12 nil nil nil] (parse-number' ".234e+12 ")))
    (is (thrown? ExceptionInfo (parse-number' ".e+12 ")))
    (is (thrown? ExceptionInfo (parse-number' "+e+12 ")))
    (is (thrown? ExceptionInfo (parse-number' "-e+12 ")))
    (is (thrown? ExceptionInfo (parse-number' "+.e+12 ")))
    (is (thrown? ExceptionInfo (parse-number' "1e- ")))
    (is (thrown? ExceptionInfo (parse-number' "+1e ")))
    (is (thrown? ExceptionInfo (parse-number' "+e1 ")))
    (is (thrown? ExceptionInfo (parse-number' ". ")))
    (is (thrown? ExceptionInfo (parse-number' "-. ")))))


(defn parse-string' [s] (parse-string (position-reader (subs s 2)) (second s) (first s)))

(deftest string-test
  (testing "Parsing short string literals"
    (is (= [:eof ""] (parse-string' "\"\"")))
    (is (= [:eof "hello world"] (parse-string' "\"hello world\"")))
    (is (= [:eof "hello world"] (parse-string' "'hello world'")))
    (is (= [\space "hello\nworld"] (parse-string' "\"hello\\nworld\" ")))
    (is (= [\space "hello \"world\""] (parse-string' "\"hello \\\"world\\\"\" ")))
    (is (= [\space "hello 'world'"] (parse-string' "'hello \\'world\\'' ")))
    (is (= [:eof "hello wörld"] (parse-string' "'hello w\\u00f6rld'")))
    (is (= [:eof "hello w🫤rld"] (parse-string' "'hello w\\U0001FAE4rld'")))
    (is (thrown? ExceptionInfo (parse-string' "'hello w\\U0001FAE4rld\" ")))))

(defn parse-long-string' [s] (parse-long-string (position-reader (subs s 4)) (nth s 3) (first s)))

(deftest long-string-test
  (testing "Parsing long string literals"
    (is (= [:eof ""] (parse-long-string' "\"\"\"\"\"\"")))
    (is (= [:eof "hello world"] (parse-long-string' "\"\"\"hello world\"\"\"")))
    (is (= [:eof "hello world"] (parse-long-string' "'''hello world'''")))
    (is (= [\space "hello\nworld"] (parse-long-string' "\"\"\"hello\\nworld\"\"\" ")))
    (is (= [\space "hello \"world\" "] (parse-long-string' "\"\"\"hello \"world\" \"\"\" ")))
    (is (= [\space "hello 'world' "] (parse-long-string' "'''hello 'world' ''' ")))
    (is (= [\' "hello 'world"] (parse-long-string' "'''hello 'world'''' ")))
    (is (= [:eof "hello wörld"] (parse-long-string' "'''hello w\\u00f6rld'''")))
    (is (= [:eof "hello w🫤rld"] (parse-long-string' "'''hello w\\U0001FAE4rld'''")))
    (is (= [:eof "hello '' wörld"] (parse-long-string' "'''hello '' w\\u00f6rld'''")))
    (is (thrown? ExceptionInfo (parse-long-string' "'''hello w\\U0001FAE4rld\"'' ")))))

  (defn parse-lang-tag' [s gen t lit] (parse-lang-tag (position-reader (subs s 1)) (first s) gen t lit))

(deftest lang-tag-test
  (testing "Parsing language tags"
    (let [g (new-generator)]
      (is (= [\space (new-lang-string g "" "en") g nil]
             (parse-lang-tag' "en " g nil "")))
      (is (= [:eof (new-lang-string g "" "en-uk") g nil]
             (parse-lang-tag' "en-uk" g nil "")))
      (is (= [\. (new-lang-string g "" "en-uk-scot") g nil]
             (parse-lang-tag' "en-uk-scot." g nil "")))
      (is (= [\space (new-lang-string g "" "en-uk-scot-nth2") g nil]
             (parse-lang-tag' "en-uk-scot-nth2 " g nil "")))
      (is (= [\1 (new-lang-string g "" "en") g nil]
             (parse-lang-tag' "en1-uk-scot-nth2 " g nil "")))
      (is (thrown? ExceptionInfo (parse-lang-tag' "en-uk-scot- " g nil "")))
      (is (thrown? ExceptionInfo (parse-lang-tag' "en-uk- " g nil "")))
      (is (thrown? ExceptionInfo (parse-lang-tag' "en- " g nil "")))
      (is (thrown? ExceptionInfo (parse-lang-tag' "- " g nil ""))))))

(defrecord TestGenerator [counter bnode-cache namespaces]
  core/NodeGenerator
  (new-node [this] [(update this :counter inc) (rdf/unsafe-blank-node (str "b" counter))])
  (new-node [this label]
    (if-let [node (get bnode-cache label)]
      [this node]
      (let [node (rdf/unsafe-blank-node (str "b" counter))]
        [(-> this (update :counter inc) (update :bnode-cache assoc label node)) node])))
  (add-base [this iri] (update this :namespaces assoc :base (rdf/as-str iri)))
  (add-prefix [this prefix iri] (update this :namespaces assoc prefix (rdf/as-str iri)))
  (iri-for [this prefix] (get namespaces prefix))
  (get-namespaces [this] (dissoc namespaces :base))
  (get-base [this] (:base namespaces))
  (new-qname [this prefix local] (rdf/iri (str (get namespaces prefix) local) prefix local))
  (new-iri [this iri] (rdf/iri iri))
  (new-literal [this s] (rdf/typed-literal s rdf/XSD-STRING))
  (new-literal [this s t] (rdf/typed-literal s t))
  (new-lang-string [this s lang] (rdf/lang-literal s lang))
  (rdf-type [this] RDF-TYPE)
  (rdf-first [this] RDF-FIRST)
  (rdf-rest [this] RDF-REST)
  (rdf-nil [this] RDF-NIL))

(defn test-generator [] (->TestGenerator 0 {} EMPTY_MAP))

(defn parse-literal'
  [s g] (parse-literal (position-reader (subs s 1)) (first s) g nil))

(deftest string-literal-test
  (testing "Parsing string literals"
    (let [g (-> (new-generator) (add-prefix "xsd" "http://xsd.org/"))]
      (is (= [:eof "" g nil] (parse-literal' "\"\"" g)))
      (is (= [:eof "hello world" g nil] (parse-literal' "\"hello world\"" g)))
      (is (= [:eof "hello world" g nil] (parse-literal' "\"\"\"hello world\"\"\"" g)))
      (is (= [:eof "hello world" g nil] (parse-literal' "'''hello world'''" g)))
      (is (= [\space "hello\nworld" g nil] (parse-literal' "\"\"\"hello\\nworld\"\"\" " g)))
      (is (= [\space "hello \"world\" " g nil] (parse-literal' "\"\"\"hello \"world\" \"\"\" " g)))
      (is (= [\space "hello 'world' " g nil] (parse-literal' "'''hello 'world' ''' " g)))
      (is (= [\' "hello 'world" g nil] (parse-literal' "'''hello 'world'''' " g)))
      (is (= [:eof "hello wörld" g nil] (parse-literal' "'''hello w\\u00f6rld'''" g)))
      (is (= [:eof "hello w🫤rld" g nil] (parse-literal' "'''hello w\\U0001FAE4rld'''" g)))
      (is (= [:eof "hello '' wörld" g nil] (parse-literal' "'''hello '' w\\u00f6rld'''" g)))
      (is (= [:eof (new-lang-string g "hello world" "en") g nil]
             (parse-literal' "\"hello world\"@en" g)))
      (is (= [\space (new-lang-string g "hello world" "en-uk") g nil]
             (parse-literal' "\"hello world\"@en-uk " g)))
      (is (= [:eof (new-literal g "hello world" (new-iri g "http://xsd.org/string")) g nil]
             (parse-literal' "\"hello world\"^^<http://xsd.org/string>" g)))
      (is (= [:eof (new-literal g "hello world"
                                   (iri "http://xsd.org/string" "xsd" "string"))
              g nil]
             (parse-literal' "\"hello world\"^^xsd:string" g)))))
  (testing "Parsing string literals with a non-default generator"
    (let [g (-> (test-generator) (add-prefix "xsd" "http://xsd.org/"))]
      (is (= [:eof (typed-literal "" XSD-STRING) g nil] (parse-literal' "\"\"" g)))
      (is (= [:eof (typed-literal "hello world" XSD-STRING) g nil] (parse-literal' "\"hello world\"" g)))
      (is (= [:eof (typed-literal "hello world" XSD-STRING) g nil] (parse-literal' "\"\"\"hello world\"\"\"" g)))
      (is (= [:eof (typed-literal "hello world" XSD-STRING) g nil] (parse-literal' "'''hello world'''" g)))
      (is (= [\space (typed-literal "hello\nworld" XSD-STRING) g nil] (parse-literal' "\"\"\"hello\\nworld\"\"\" " g)))
      (is (= [\space (typed-literal "hello \"world\" " XSD-STRING) g nil] (parse-literal' "\"\"\"hello \"world\" \"\"\" " g)))
      (is (= [\space (typed-literal "hello 'world' " XSD-STRING) g nil] (parse-literal' "'''hello 'world' ''' " g)))
      (is (= [\' (typed-literal "hello 'world" XSD-STRING) g nil] (parse-literal' "'''hello 'world'''' " g)))
      (is (= [:eof (typed-literal "hello wörld" XSD-STRING) g nil] (parse-literal' "'''hello w\\u00f6rld'''" g)))
      (is (= [:eof (typed-literal "hello w🫤rld" XSD-STRING) g nil] (parse-literal' "'''hello w\\U0001FAE4rld'''" g)))
      (is (= [:eof (typed-literal "hello '' wörld" XSD-STRING) g nil] (parse-literal' "'''hello '' w\\u00f6rld'''" g)))
      (is (= [:eof (new-lang-string g "hello world" "en") g nil]
             (parse-literal' "\"hello world\"@en" g)))
      (is (= [\space (new-lang-string g "hello world" "en-uk") g nil]
             (parse-literal' "\"hello world\"@en-uk " g)))
      (is (= [:eof (new-literal g "hello world" (new-iri g "http://xsd.org/string")) g nil]
             (parse-literal' "\"hello world\"^^<http://xsd.org/string>" g)))
      (is (= [:eof (new-literal g "hello world"
                                   (iri "http://xsd.org/string" "xsd" "string"))
              g nil]
             (parse-literal' "\"hello world\"^^xsd:string" g))))))

(defn parse-anon
  [s g]
  (let [[c node g _ _] (anon-blank-node (position-reader (subs s 1)) (first s) g nil)]
    [c node g]))

(deftest parse-anon-test
  (testing "Parsing anonymous blank nodes"
    (let [g (-> (new-generator))
          [c1 b1 g1] (parse-anon "]" g)
          [c2 b2 g2] (parse-anon "] ." g1)]
      (is (= :eof c1))
      (is (= (blank-node 0) b1))
      (is (= \space c2))
      (is (= (blank-node 1) b2)))))

(defn parse-entity
  [s g]
  (let [[c node g triples] (parse-blank-node-entity (position-reader (subs s 2)) (nth s 1) g (triple-accumulator))]
    [c node g (seq triples)]))

(deftest parse-blank-entity-test
  (testing "Parsing blank node entity"
    (let [g (-> (new-generator) (add-prefix "" "http://a.org/"))
          a (new-qname g "" "a")
          b (new-qname g "" "b")
          [c1 b1 g1 t1] (parse-entity "[:a 1; :b 2]" g)
          [c2 b2 g2 t2] (parse-entity "[:a 1, \"one\"; :b 2]." g1)]
      (is (= c1 :eof))
      (is (= b1 (blank-node 0)))
      (is (= t1 [[b1 a 1] [b1 b 2]]))
      (is (= c2 \.))
      (is (= b2 (blank-node 1)))
      (is (= t2 [[b2 a 1] [b2 a "one"] [b2 b 2]])))))

(defn parse-blank
  [s g]
  (let [[c node g _] (parse-blank-node (position-reader (subs s 1)) (first s) g nil)]
    [c node g]))

(deftest parse-blank-test
  (testing "Parsing labelled blank node"
    (let [g (new-generator)
          [c1 b1 g1] (parse-blank "_:b1" g)
          [c2 b2 g2] (parse-blank "_:b111d," g1)
          [c3 b3 g3] (parse-blank "_:x1 " g2)
          [c4 b4 g4] (parse-blank "_:b1 " g3)
          [c5 b5 g5] (parse-blank "_:b1.1d]" g4)]
      (is (= c1 :eof))
      (is (= b1 (blank-node 0)))
      (is (= c2 \,))
      (is (= b2 (blank-node 1)))
      (is (= c3 \space))
      (is (= b3 (blank-node 2)))
      (is (= c4 \space))
      (is (= b4 (blank-node 0)))
      (is (= c5 \]))
      (is (= b5 (blank-node 3))))))

(defn parse-collection'
  [s g]
  (let [[c node g triples] (parse-collection (position-reader (subs s 1)) (first s) g (triple-accumulator))]
    [c node g (seq triples)]))

(deftest collection-test
  (testing "Parsing of basic collections"
    (let [g (-> (new-generator))
          a (new-qname g "" "a")
          b (new-qname g "" "b")
          [c0 b0 g0 t0] (parse-collection' "()" g)
          [c1 b1 g1 t1] (parse-collection' "( 1 2)" g0)
          b12 (blank-node 1)
          [c2 b2 g2 t2] (parse-collection' "(:a 1 :b 2 )" g1)
                                        ;b2 (blank-node 2)
          b22 (blank-node 3)
          b23 (blank-node 4)
          b24 (blank-node 5)
          [c3 b3 g3 t3] (parse-collection' "(:a 1 \"one\" [] :b 2)." g2)
                                        ;b3 (blank-node 6)
          b32 (blank-node 7)
          b33 (blank-node 8)
          b34 (blank-node 9)
          b34a (blank-node 10)
          b35 (blank-node 11)
          b36 (blank-node 12)
          [c4 b4 g4 t4] (parse-collection' "([:a 1; :b 2] [:a 2])." g3)
                                        ;b4 (blank-node 13)
          b41a (blank-node 14)
          b42 (blank-node 15)
          b42a (blank-node 16)]
      (is (= c0 :eof))
      (is (= b0 RDF-NIL))
      (is (empty? t0))
      (is (= c1 :eof))
      (is (= b1 (blank-node 0)))
      (is (= t1 [[b1 RDF-FIRST 1] [b1 RDF-REST b12] [b12 RDF-FIRST 2] [b12 RDF-REST RDF-NIL]]))
      (is (= c2 :eof))
      (is (= b2 (blank-node 2)))
      (is (= t2 [[b2 RDF-FIRST a] [b2 RDF-REST b22] [b22 RDF-FIRST 1] [b22 RDF-REST b23]
                 [b23 RDF-FIRST b] [b23 RDF-REST b24] [b24 RDF-FIRST 2] [b24 RDF-REST RDF-NIL]]))
      (is (= c3 \.))
      (is (= b3 (blank-node 6)))
      (is (= t3 [[b3 RDF-FIRST a] [b3 RDF-REST b32] [b32 RDF-FIRST 1] [b32 RDF-REST b33]
                 [b33 RDF-FIRST "one"] [b33 RDF-REST b34] [b34 RDF-FIRST b34a] [b34 RDF-REST b35]
                 [b35 RDF-FIRST b] [b35 RDF-REST b36] [b36 RDF-FIRST 2] [b36 RDF-REST RDF-NIL]]))
      (is (= c4 \.))
      (is (= b4 (blank-node 13)))
      (is (= t4 [[b41a a 1] [b41a b 2] [b4 RDF-FIRST b41a] [b4 RDF-REST b42] [b42a a 2]
                 [b42 RDF-FIRST b42a] [b42 RDF-REST RDF-NIL]])))))

(defn parse-pred-obj-list
  [s g]
  (let [[c gen triples] (parse-predicate-object-list (position-reader (subs s 1)) (first s) :s g (triple-accumulator))]
    [c gen (seq triples)]))

(deftest predicate-object-list-test
  (testing "Parsing of predicate/object pairs"
    (let [g (-> (new-generator) (add-prefix "" "http://a.org/"))
          a (new-qname g "" "a")
          b (new-qname g "" "b")]
      (is (= [\. g nil] (parse-pred-obj-list "." g)))
      (is (= [\. g [[:s a b]]] (parse-pred-obj-list ":a :b ." g)))
      (is (= [\] g [[:s a b]]] (parse-pred-obj-list ":a :b ]" g)))
      (is (= [\. g [[:s a b]]] (parse-pred-obj-list ":a :b." g)))
      (is (= [\] g [[:s a b]]] (parse-pred-obj-list ":a :b]" g)))
      (is (= [\. g [[:s a b] [:s b 2.0]]] (parse-pred-obj-list ":a :b ;\n:b 2.." g)))
      (is (= [\] g [[:s a b] [:s b 3]]] (parse-pred-obj-list ":a :b ;\n :b 3]" g)))
      (is (= [\] g [[:s a b] [:s b 3]]] (parse-pred-obj-list ":a :b ;\n :b 3 ;]" g)))
      (is (= [\] g [[:s a b] [:s b 3] [:s b 2]]] (parse-pred-obj-list ":a :b ;\n :b 3, 2,]" g)))
      (is (= [\. g [[:s a b] [:s b 2] [:s a "x"]]]
             (parse-pred-obj-list ":a :b ;\n:b 2;:a \"x\"." g)))
      (is (= [\. g [[:s a b] [:s a 2]]] (parse-pred-obj-list ":a :b ,\n 2 ." g)))
      (is (= [\] g [[:s a 1] [:s a 2] [:s a 3] [:s b 4] [:s b 5]]]
             (parse-pred-obj-list ":a 1,2,3;:b 4,5]" g))))))

#?(:cljs (cljs.test/run-tests))
