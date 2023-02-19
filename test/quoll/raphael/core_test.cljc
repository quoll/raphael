(ns quoll.raphael.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [quoll.raphael.core :refer [skip-whitespace skip-to dot? newline? add-base
                                        parse-iri-ref add-prefix new-generator parse-statement
                                        parse-local parse-prefixed-name parse-number parse-string
                                        parse-long-string parse-literal
                                        anon-blank-node parse-blank-node-entity parse-blank-node
                                        new-lang-string new-literal new-iri
                                        ->BlankNode ->Iri new-qname
                                        RDF-NIL RDF-FIRST RDF-REST RDF-TYPE
                                        parse-predicate-object-list parse-collection
                                        parse]]
            [quoll.raphael.text :refer [char-at]])
  #?(:clj (:import [clojure.lang ExceptionInfo])))

#?(:clj (set! *warn-on-reflection* true))

(defn skip-whitespace' [s n] (skip-whitespace s n (char-at s n)))

(deftest ws-test
  (testing "Tests if whitespace is skipped correctly."
    (is (= [3 \a] (skip-whitespace' "   a b" 0)))
    (is (= [3 \a] (skip-whitespace' "   a b" 2)))
    (is (= [3 \a] (skip-whitespace' "   a b" 3)))
    (is (= [5 \b] (skip-whitespace' "   a b" 4)))
    (is (= [6 :eof] (skip-whitespace' "   a b" 6)))))

(defn skip-to' [s n chars] (skip-to s n (char-at s n) chars))

(deftest skip-to-test
  (testing "Testing skipping whitespace to a character"
    (is (= [4 \space] (skip-to' "   . a" 0 dot?)))
    (is (= [4 \space] (skip-to' "   . a" 1 dot?)))
    (is (= [4 \space] (skip-to' "   . a" 3 dot?)))
    (is (thrown? ExceptionInfo (skip-to' "   . a" 4 dot?)))
    (is (= [4 \space] (skip-to' "   \n a" 0 newline?)))
    (is (= [4 \space] (skip-to' "   \n a" 1 newline?)))
    (is (= [4 \space] (skip-to' "   \n a" 3 newline?)))
    (is (thrown? ExceptionInfo (skip-to' "   \n a" 4 newline?)))))

(defn parse-iri-ref' [s n c gen] (parse-iri-ref s n c gen nil))

(deftest iri-ref-test
  (testing "Parsing an IRI reference"
    (let [g (-> (new-generator) (add-base "http://test.org/"))
          i (fn [s] (new-iri g s))]
      (is (= [16 :eof (i "http://ex.com/") g nil]
             (parse-iri-ref' "<http://ex.com/>" 0 \< g)))
      (is (= [46 :eof (i "http://example.com/path?query=x&y=2#fragment") g nil]
             (parse-iri-ref' "<http://example.com/path?query=x&y=2#fragment>" 0 \< g)))
      (is (= [20 :eof (i "http://ex.com/") g nil]
             (parse-iri-ref' "foo <http://ex.com/>" 4 \< g)))
      (is (= [50 :eof (i "http://example.com/path?query=x&y=2#fragment") g nil]
             (parse-iri-ref' "foo <http://example.com/path?query=x&y=2#fragment>" 4 \< g)))
      (is (thrown? ExceptionInfo
                   (parse-iri-ref' "<http://example.com/path query=x&y=2#fragment>" 0 \< g)))
      (is (thrown? ExceptionInfo
                   (parse-iri-ref' "<http://example.com/path?query=x&y=2#fragment>" 1 \h g)))
      (is (thrown? ExceptionInfo
                   (parse-iri-ref' "<http://example.com/path?query=x&y=2#fragment" 0 \< g)))
      (let [ge (new-generator)]
        (is (= [6 :eof (new-iri ge "path") ge nil] (parse-iri-ref' "<path>" 0 \< ge))))
      (is (= [6 :eof (i "http://test.org/path") g nil] (parse-iri-ref' "<path>" 0 \< g))))))

(defn parse-statement' [s n gen] (parse-statement s n gen nil))
(defn parse-statement-t [s gen]
  (let [[n c g t] (parse-statement s 0 (char-at s 0) gen (transient []))]
    [n c g (persistent! t)]))

(deftest non-triple-statement-test
  (testing "parsing statements that are not triples"
    (let [g (new-generator)
          [n1 c1 g1 t1] (parse-statement' "BASE <http://test.org/>\n" 0 g)
          [n2 c2 g2 t2] (parse-statement' "@base <http://test.com/>. \n" 0 g1)
          [n3 c3 g3 t3] (parse-statement' "PREFIX test: <http://test.org/>\n" 0 g2)
          [n4 c4 g4 t4] (parse-statement' "@prefix t2: <http://test.com/>. \n" 0 g3)
          [n5 c5 g5 t5] (parse-statement' "@prefix t\ud800\udd49: <http://atticten.com/>. \n" 0 g4)
          [n6 c6 g6 t6] (parse-statement' "@prefix \ud800\udd49x: <http://atticten.com/pre>. \n" 0 g5)
          [n7 c7 g7 t7] (parse-statement' "@prefix \udb7f\udfffx: <http://unicode.com/limit>. \n" 0 g6)
          [n8 c8 g8 t8] (parse-statement' "@prefix : <http://short.com/>. \n" 0 g7)]
      (is (= 24 n1))
      (is (= :eof c1))
      (is (= {:base "http://test.org/"} (:namespaces g1)))
      (is (nil? t1))
      (is (= 25 n2))
      (is (= \space c2))
      (is (= {:base "http://test.com/"} (:namespaces g2)))
      (is (nil? t2))
      (is (= 32 n3))
      (is (= :eof c3))
      (is (= {:base "http://test.com/" "test" "http://test.org/"} (:namespaces g3)))
      (is (nil? t3))
      (is (= 31 n4))
      (is (= \space c4))
      (is (= {:base "http://test.com/" "test" "http://test.org/" "t2" "http://test.com/"} (:namespaces g4)))
      (is (nil? t4))

      (is (= 36 n5))
      (is (= \space c5))
      (is (= {:base "http://test.com/" "test" "http://test.org/" "t2" "http://test.com/"
              "t\ud800\udd49" "http://atticten.com/"}
             (:namespaces g5)))
      (is (nil? t5))
      (is (= 39 n6))
      (is (= \space c6))
      (is (= {:base "http://test.com/" "test" "http://test.org/" "t2" "http://test.com/"
              "t\ud800\udd49" "http://atticten.com/" "\ud800\udd49x" "http://atticten.com/pre"}
             (:namespaces g6)))
      (is (nil? t6))
      (is (= 40 n7))
      (is (= \space c7))
      (is (= {:base "http://test.com/" "test" "http://test.org/" "t2" "http://test.com/"
              "t\ud800\udd49" "http://atticten.com/" "\ud800\udd49x" "http://atticten.com/pre"
              "\udb7f\udfffx" "http://unicode.com/limit"}
             (:namespaces g7)))
      (is (nil? t7))
      (is (= 30 n8))
      (is (= \space c8))
      (is (= {:base "http://test.com/" "test" "http://test.org/" "t2" "http://test.com/"
              "t\ud800\udd49" "http://atticten.com/" "\ud800\udd49x" "http://atticten.com/pre"
              "\udb7f\udfffx" "http://unicode.com/limit" "" "http://short.com/"}
             (:namespaces g8)))
      (is (nil? t8))


      (is (thrown? ExceptionInfo
                   (parse-statement' "@prefix \ud800\ud849x: <http://atticten.com/pre>. \n" 0 g5)))
      (is (thrown? ExceptionInfo
                   (parse-statement' "@prefix \ud800x: <http://atticten.com/pre>. \n" 0 g5)))
      (is (thrown? ExceptionInfo
                   (parse-statement' "@prefix \udd00\udd49x: <http://atticten.com/pre>. \n" 0 g5)))
      (is (thrown? ExceptionInfo
                   (parse-statement' "@prefix \udb80\udc00x: <http://atticten.com/pre>. \n" 0 g5))))))

(deftest triple-statement-test
  (testing "parsing statements that are not triples"
    (let [g (new-generator)
          [n1 c1 g1 t1] (parse-statement-t "BASE <http://test.org/>\n" g)
          [n2 c2 g2 t2] (parse-statement-t "PREFIX a: <http://a.org/>\n" g1)
          [n3 c3 g3 t3] (parse-statement-t "a:a a:b 'test' ." g2)
          [n4 c4 g4 t4] (parse-statement-t "a:a <b> (1 2) . " g3)
          [n5 c5 g5 t5] (parse-statement-t "[a:a 1; a:b 2] a a:X . " g3)
          [n6 c6 g6 t6] (parse-statement-t "[a:a 1;a:b 2;a a:X].[" g3)]
      (is (= n3 16))
      (is (= c3 :eof))
      (is (= (:namespaces g3) {:base "http://test.org/" "a" "http://a.org/"}))
      (is (= t3 [[(new-qname g3 "a" "a") (new-qname g3 "a" "b") "test"]]))
      (is (= n4 15))
      (is (= c4 \space))
      (is (= t4 [[(->BlankNode 0) RDF-FIRST 1]
                 [(->BlankNode 0) RDF-REST (->BlankNode 1)]
                 [(->BlankNode 1) RDF-FIRST 2]
                 [(->BlankNode 1) RDF-REST RDF-NIL]
                 [(new-qname g3 "a" "a") (new-iri g3 "http://test.org/b") (->BlankNode 0)]]))
      (is (= n5 22))
      (is (= c5 \space))
      (is (= t5 [[(->BlankNode 0) (new-qname g3 "a" "a") 1]
                 [(->BlankNode 0) (new-qname g3 "a" "b") 2]
                 [(->BlankNode 0) RDF-TYPE (new-qname g3 "a" "X")]]))
      (is (= n6 20))
      (is (= c6 \[))
      (is (= t6 [[(->BlankNode 0) (new-qname g3 "a" "a") 1]
                 [(->BlankNode 0) (new-qname g3 "a" "b") 2]
                 [(->BlankNode 0) RDF-TYPE (new-qname g3 "a" "X")]])))))

(deftest parse-comment-test
  (testing "parsing statements that have comments"
    (let [g (new-generator)
          [n1 c1 g1 t1] (parse-statement' "@base <http://test.org/> # comment\n. \n" 0 g)
          [n2 c2 g2 t2] (parse-statement' "@prefix a: #comment\n  <http://test.com/>. \n" 0 g1)
          [n3 c3 g3 t3] (parse-statement' "#comment\n" 0 g2)
          [n4 c4 g4 t4] (parse-statement-t "a:a #comment\na:b            #comment\n 'test'#comment\n." g2)]
      (is (= n1 36))
      (is (= c1 \space))
      (is (= (:namespaces g1) {:base "http://test.org/"}))
      (is (= n2 41))
      (is (= c2 \space))
      (is (= (:namespaces g2) {:base "http://test.org/" "a" "http://test.com/"}))
      (is (= n3 9))
      (is (= c3 :eof))
      (is (nil? t3))
      (is (= n4 54))
      (is (= c4 :eof))
      (is (= t4 [[(new-qname g2 "a" "a") (new-qname g2 "a" "b") "test"]])))))


(deftest local-name-test
  (testing "Parsing the local portion of a prefixed name"
    (is (= [5 \space "alice"] (parse-local "alice " 0)))
    (is (= [6 \space "alice"] (parse-local " alice " 1)))
    (is (= [5 \. "alice"] (parse-local "alice. " 0)))
    (is (= [7 \space "alice.b"] (parse-local "alice.b " 0)))
    (is (= [5 \. "alice"] (parse-local "alice. " 0)))
    (is (= [9 \space "alice%20b"] (parse-local "alice%20b " 0)))
    (is (= [8 \space "alice!b"] (parse-local "alice\\!b " 0)))
    (is (= [9 \space "alice%ba"] (parse-local "alice\\%ba " 0)))
    (is (thrown? ExceptionInfo (parse-local "alice%%b " 0)))
    (is (thrown? ExceptionInfo (parse-local "alice%bg " 0)))
    (is (thrown? ExceptionInfo (parse-local "alice\\bg " 0)))))

(defn parse-prefixed-name' [s n c gen] (parse-prefixed-name s n c gen nil))

(deftest prefixed-name-test
  (testing "Parsing prefixed names"
    (let [g (-> (new-generator)
                (add-prefix "ex" "http://ex.com/")
                (add-prefix "" "http://test.org/")
                (add-prefix "a-b-c" "http://t1.org/")
                (add-prefix "a_b" "http://t2.org/")
                (add-prefix "a.b" "http://t3.org/"))]
      (is (= [6 \space (->Iri "ex" "cat" "http://ex.com/cat") g nil]
             (parse-prefixed-name' "ex:cat " 0 \e g)))
      (is (= [6 \. (->Iri "ex" "cat" "http://ex.com/cat") g nil]
             (parse-prefixed-name' "ex:cat. " 0 \e g)))
      (is (= [4 \space (->Iri "" "cat" "http://test.org/cat") g nil]
             (parse-prefixed-name' ":cat " 0 \: g)))
      (is (= [4 \. (->Iri "" "cat" "http://test.org/cat") g nil]
             (parse-prefixed-name' ":cat. " 0 \: g)))
      (is (= [9 \space (->Iri "a-b-c" "cat" "http://t1.org/cat") g nil]
             (parse-prefixed-name' "a-b-c:cat " 0 \a g)))
      (is (= [7 \space (->Iri "a_b" "cat" "http://t2.org/cat") g nil]
             (parse-prefixed-name' "a_b:cat " 0 \a g)))
      (is (= [7 \space (->Iri "a.b" "cat" "http://t3.org/cat") g nil]
             (parse-prefixed-name' "a.b:cat " 0 \a g)))
      (is (thrown? ExceptionInfo (parse-prefixed-name' "_b:cat " 0 \_ g)))
      (is (thrown? ExceptionInfo (parse-prefixed-name' ".b:cat " 0 \. g)))
      (is (thrown? ExceptionInfo (parse-prefixed-name' "-b:cat " 0 \- g))))))

(defn parse-number' [s n] (parse-number s n (char-at s n) nil nil))

(deftest number-test
  (testing "Parsing numeric literals"
    (is (= [3 :eof 123 nil nil] (parse-number' "123" 0)))
    (is (= [3 \space 123 nil nil] (parse-number' "123 " 0)))
    (is (= [4 \space 123 nil nil] (parse-number' " 123 " 1)))
    (is (= [1 \space 0 nil nil] (parse-number' "0 " 0)))
    (is (= [2 \space 0.0 nil nil] (parse-number' "0. " 0)))
    (is (= [5 \space 0.234 nil nil] (parse-number' "0.234 " 0)))
    (is (= [4 \space 0.234 nil nil] (parse-number' ".234 " 0)))
    (is (= [5 \space 23.4 nil nil] (parse-number' "23.40 " 0)))
    (is (= [4 \space 234.0 nil nil] (parse-number' "234. " 0)))
    (is (= [4 :eof -123 nil nil] (parse-number' "-123" 0)))
    (is (= [4 \space 123 nil nil] (parse-number' "+123 " 0)))
    (is (= [5 \space -123 nil nil] (parse-number' " -123 " 1)))
    (is (= [2 \space 0 nil nil] (parse-number' "+0 " 0)))
    (is (= [3 \space 0.0 nil nil] (parse-number' "-0. " 0)))
    (is (= [6 \space 0.234 nil nil] (parse-number' "+0.234 " 0)))
    (is (= [5 \space -0.234 nil nil] (parse-number' "-.234 " 0)))
    (is (= [6 \space 23.4 nil nil] (parse-number' "+23.40 " 0)))
    (is (= [5 \space -234.0 nil nil] (parse-number' "-234. " 0)))
    (is (= [6 \space 23.4e2 nil nil] (parse-number' "23.4e2 " 0)))
    (is (= [6 \space 234.0e2 nil nil] (parse-number' "234.e2 " 0)))
    (is (= [7 \space -234.0e2 nil nil] (parse-number' "-234.e2 " 0)))
    (is (= [7 \space -23.4e2 nil nil] (parse-number' "-23.4e2 " 0)))
    (is (= [8 \space 0.234e12 nil nil] (parse-number' "+.234e12 " 0)))
    (is (= [7 \space 0.234e12 nil nil] (parse-number' ".234e12 " 0)))
    (is (= [9 \space 0.234e-12 nil nil] (parse-number' "+.234e-12 " 0)))
    (is (= [8 \space 0.234e12 nil nil] (parse-number' ".234e+12 " 0)))
    (is (thrown? ExceptionInfo (parse-number' ".e+12 " 0)))
    (is (thrown? ExceptionInfo (parse-number' "+e+12 " 0)))
    (is (thrown? ExceptionInfo (parse-number' "-e+12 " 0)))
    (is (thrown? ExceptionInfo (parse-number' "+.e+12 " 0)))
    (is (thrown? ExceptionInfo (parse-number' "1e- " 0)))
    (is (thrown? ExceptionInfo (parse-number' "+1e " 0)))
    (is (thrown? ExceptionInfo (parse-number' "+e1 " 0)))
    (is (thrown? ExceptionInfo (parse-number' ". " 0)))
    (is (thrown? ExceptionInfo (parse-number' "-. " 0)))))

(deftest string-test
  (testing "Parsing short string literals"
    (is (= [2 :eof ""] (parse-string \" "\"\"" 1 \")))
    (is (= [13 :eof "hello world"] (parse-string \" "\"hello world\"" 1 \h)))
    (is (= [13 :eof "hello world"] (parse-string \' "'hello world'" 1 \h)))
    (is (= [14 \space "hello\nworld"] (parse-string \" "\"hello\\nworld\" " 1 \h)))
    (is (= [17 \space "hello \"world\""] (parse-string \" "\"hello \\\"world\\\"\" " 1 \h)))
    (is (= [17 \space "hello 'world'"] (parse-string \' "'hello \\'world\\'' " 1 \h)))
    (is (= [18 :eof "hello wörld"] (parse-string \' "'hello w\\u00f6rld'" 1 \h)))
    (is (= [22 :eof "hello w🫤rld"] (parse-string \' "'hello w\\U0001FAE4rld'" 1 \h)))
    (is (thrown? ExceptionInfo (parse-string \' "'hello w\\U0001FAE4rld\" " 1 \h)))))

(deftest long-string-test
  (testing "Parsing short string literals"
    (is (= [6 :eof ""] (parse-long-string \" "\"\"\"\"\"\"" 3 \")))
    (is (= [17 :eof "hello world"] (parse-long-string \" "\"\"\"hello world\"\"\"" 3 \h)))
    (is (= [17 :eof "hello world"] (parse-long-string \' "'''hello world'''" 3 \h)))
    (is (= [18 \space "hello\nworld"] (parse-long-string \" "\"\"\"hello\\nworld\"\"\" " 3 \h)))
    (is (= [20 \space "hello \"world\" "] (parse-long-string \" "\"\"\"hello \"world\" \"\"\" " 3 \h)))
    (is (= [20 \space "hello 'world' "] (parse-long-string \' "'''hello 'world' ''' " 3 \h)))
    (is (= [18 \' "hello 'world"] (parse-long-string \' "'''hello 'world'''' " 3 \h)))
    (is (= [22 :eof "hello wörld"] (parse-long-string \' "'''hello w\\u00f6rld'''" 3 \h)))
    (is (= [26 :eof "hello w🫤rld"] (parse-long-string \' "'''hello w\\U0001FAE4rld'''" 3 \h)))
    (is (= [25 :eof "hello '' wörld"] (parse-long-string \' "'''hello '' w\\u00f6rld'''" 3 \h)))
    (is (thrown? ExceptionInfo (parse-long-string \' "'''hello w\\U0001FAE4rld\"'' " 3 \h)))))


(defn parse-literal'
  ([s g] (parse-literal s 0 (char-at s 0) g nil))
  ([s n g] (parse-literal s n (char-at s n) g nil)))

(deftest string-literal-test
  (testing "Parsing string literals"
    (let [g (-> (new-generator) (add-prefix "xsd" "http://xsd.org/"))]
      (is (= [2 :eof "" g nil] (parse-literal' "\"\"" g)))
      (is (= [3 :eof "" g nil] (parse-literal' " \"\"" 1 g)))
      (is (= [13 :eof "hello world" g nil] (parse-literal' "\"hello world\"" g)))
      (is (= [17 :eof "hello world" g nil] (parse-literal' "\"\"\"hello world\"\"\"" g)))
      (is (= [17 :eof "hello world" g nil] (parse-literal' "'''hello world'''" g)))
      (is (= [18 \space "hello\nworld" g nil] (parse-literal' "\"\"\"hello\\nworld\"\"\" " g)))
      (is (= [20 \space "hello \"world\" " g nil] (parse-literal' "\"\"\"hello \"world\" \"\"\" " g)))
      (is (= [20 \space "hello 'world' " g nil] (parse-literal' "'''hello 'world' ''' " g)))
      (is (= [18 \' "hello 'world" g nil] (parse-literal' "'''hello 'world'''' " g)))
      (is (= [22 :eof "hello wörld" g nil] (parse-literal' "'''hello w\\u00f6rld'''" g)))
      (is (= [26 :eof "hello w🫤rld" g nil] (parse-literal' "'''hello w\\U0001FAE4rld'''" g)))
      (is (= [25 :eof "hello '' wörld" g nil] (parse-literal' "'''hello '' w\\u00f6rld'''" g)))
      (is (= [16 :eof (new-lang-string g "hello world" "en") g nil]
             (parse-literal' "\"hello world\"@en" g)))
      (is (= [19 \space (new-lang-string g "hello world" "en-uk") g nil]
             (parse-literal' "\"hello world\"@en-uk " g)))
      (is (= [38 :eof (new-literal g "hello world" (new-iri g "http://xsd.org/string")) g nil]
             (parse-literal' "\"hello world\"^^<http://xsd.org/string>" g)))
      (is (= [25 :eof (new-literal g "hello world"
                                   (->Iri "xsd" "string" "http://xsd.org/string"))
              g nil]
             (parse-literal' "\"hello world\"^^xsd:string" g))))))

(defn parse-anon
  [s g]
  (let [[n c node g _ _] (anon-blank-node s 1 (char-at s 1) g nil)]
    [n c node g]))

(deftest parse-anon-test
  (testing "Parsing anonymous blank nodes"
    (let [g (-> (new-generator))
          [n1 c1 b1 g1] (parse-anon "[]" g)
          [n2 c2 b2 g2] (parse-anon " ] ." g1)
          ]
      (is (= 2 n1))
      (is (= :eof c1))
      (is (= (->BlankNode 0) b1))
      (is (= 2 n2))
      (is (= \space c2))
      (is (= (->BlankNode 1) b2)))))

(defn parse-entity
  [s g]
  (let [[n c node g triples] (parse-blank-node-entity s 1 (char-at s 1) g (transient []))]
    [n c node g (persistent! triples)]))

(deftest parse-blank-entity-test
  (testing "Parsing blank node entity"
    (let [g (-> (new-generator) (add-prefix "" "http://a.org/"))
          a (new-qname g "" "a")
          b (new-qname g "" "b")
          [n1 c1 b1 g1 t1] (parse-entity "[:a 1; :b 2]" g)
          [n2 c2 b2 g2 t2] (parse-entity "[:a 1, \"one\"; :b 2]." g1)
          ]
      (is (= n1 12))
      (is (= c1 :eof))
      (is (= b1 (->BlankNode 0)))
      (is (= t1 [[b1 a 1] [b1 b 2]]))
      (is (= n2 19))
      (is (= c2 \.))
      (is (= b2 (->BlankNode 1)))
      (is (= t2 [[b2 a 1] [b2 a "one"] [b2 b 2]])))))

(defn parse-blank
  [s g]
  (let [[n c node g _] (parse-blank-node s 0 (char-at s 0) g nil)]
    [n c node g]))

(deftest parse-blank-test
  (testing "Parsing labelled blank node"
    (let [g (new-generator)
          [n1 c1 b1 g1] (parse-blank "_:b1" g)
          [n2 c2 b2 g2] (parse-blank "_:b111d," g1)
          [n3 c3 b3 g3] (parse-blank "_:x1 " g2)
          [n4 c4 b4 g4] (parse-blank "_:b1 " g3)
          [n5 c5 b5 g5] (parse-blank "_:b1.1d]" g4)]
      (is (= n1 4))
      (is (= c1 :eof))
      (is (= b1 (->BlankNode 0)))
      (is (= n2 7))
      (is (= c2 \,))
      (is (= b2 (->BlankNode 1)))
      (is (= n3 4))
      (is (= c3 \space))
      (is (= b3 (->BlankNode 2)))
      (is (= n4 4))
      (is (= c4 \space))
      (is (= b4 (->BlankNode 0)))
      (is (= n5 7))
      (is (= c5 \]))
      (is (= b5 (->BlankNode 3))))))

(defn parse-collection'
  [s g]
  (let [[n c node g triples] (parse-collection s 0 (char-at s 0) g (transient []))]
    [n c node g (persistent! triples)]))

(deftest collection-test
  (testing "Parsing of basic collections"
    (let [g (-> (new-generator))
          a (new-qname g "" "a")
          b (new-qname g "" "b")
          [n0 c0 b0 g0 t0] (parse-collection' "()" g)
          [n1 c1 b1 g1 t1] (parse-collection' "( 1 2)" g0)
          b12 (->BlankNode 1)
          [n2 c2 b2 g2 t2] (parse-collection' "(:a 1 :b 2 )" g1)
                                        ;b2 (->BlankNode 2)
          b22 (->BlankNode 3)
          b23 (->BlankNode 4)
          b24 (->BlankNode 5)
          [n3 c3 b3 g3 t3] (parse-collection' "(:a 1 \"one\" [] :b 2)." g2)
                                        ;b3 (->BlankNode 6)
          b32 (->BlankNode 7)
          b33 (->BlankNode 8)
          b34 (->BlankNode 9)
          b34a (->BlankNode 10)
          b35 (->BlankNode 11)
          b36 (->BlankNode 12)
          [n4 c4 b4 g4 t4] (parse-collection' "([:a 1; :b 2] [:a 2])." g3)
                                        ;b4 (->BlankNode 13)
          b41a (->BlankNode 14)
          b42 (->BlankNode 15)
          b42a (->BlankNode 16)]
      (is (= n0 2))
      (is (= c0 :eof))
      (is (= b0 RDF-NIL))
      (is (= t0 []))
      (is (= n1 6))
      (is (= c1 :eof))
      (is (= b1 (->BlankNode 0)))
      (is (= t1 [[b1 RDF-FIRST 1] [b1 RDF-REST b12] [b12 RDF-FIRST 2] [b12 RDF-REST RDF-NIL]]))
      (is (= n2 12))
      (is (= c2 :eof))
      (is (= b2 (->BlankNode 2)))
      (is (= t2 [[b2 RDF-FIRST a] [b2 RDF-REST b22] [b22 RDF-FIRST 1] [b22 RDF-REST b23]
                 [b23 RDF-FIRST b] [b23 RDF-REST b24] [b24 RDF-FIRST 2] [b24 RDF-REST RDF-NIL]]))
      (is (= n3 20))
      (is (= c3 \.))
      (is (= b3 (->BlankNode 6)))
      (is (= t3 [[b3 RDF-FIRST a] [b3 RDF-REST b32] [b32 RDF-FIRST 1] [b32 RDF-REST b33]
                 [b33 RDF-FIRST "one"] [b33 RDF-REST b34] [b34 RDF-FIRST b34a] [b34 RDF-REST b35]
                 [b35 RDF-FIRST b] [b35 RDF-REST b36] [b36 RDF-FIRST 2] [b36 RDF-REST RDF-NIL]]))
      (is (= n4 21))
      (is (= c4 \.))
      (is (= b4 (->BlankNode 13)))
      (is (= t4 [[b41a a 1] [b41a b 2] [b4 RDF-FIRST b41a] [b4 RDF-REST b42] [b42a a 2]
                 [b42 RDF-FIRST b42a] [b42 RDF-REST RDF-NIL]])))))

(defn parse-pred-obj-list
  [s n g]
  (let [[n c gen triples] (parse-predicate-object-list s n (char-at s n) :s g (transient []))]
    [n c gen (persistent! triples)]))

(deftest predicate-object-list-test
  (testing "Parsing of predicate/object pairs"
    (let [g (-> (new-generator) (add-prefix "" "http://a.org/"))
          a (new-qname g "" "a")
          b (new-qname g "" "b")]
      (is (= [0 \. g []] (parse-pred-obj-list "." 0 g)))
      (is (= [6 \. g [[:s a b]]] (parse-pred-obj-list ":a :b ." 0 g)))
      (is (= [6 \] g [[:s a b]]] (parse-pred-obj-list ":a :b ]" 0 g)))
      (is (= [5 \. g [[:s a b]]] (parse-pred-obj-list ":a :b." 0 g)))
      (is (= [5 \] g [[:s a b]]] (parse-pred-obj-list ":a :b]" 0 g)))
      (is (= [13 \. g [[:s a b] [:s b 2.0]]] (parse-pred-obj-list ":a :b ;\n:b 2.." 0 g)))
      (is (= [13 \] g [[:s a b] [:s b 3]]] (parse-pred-obj-list ":a :b ;\n :b 3]" 0 g)))
      (is (= [15 \] g [[:s a b] [:s b 3]]] (parse-pred-obj-list ":a :b ;\n :b 3 ;]" 0 g)))
      (is (= [19 \. g [[:s a b] [:s b 2] [:s a "x"]]]
             (parse-pred-obj-list ":a :b ;\n:b 2;:a \"x\"." 0 g)))
      (is (= [11 \. g [[:s a b] [:s a 2]]] (parse-pred-obj-list ":a :b ,\n 2 ." 0 g)))
      (is (= [15 \] g [[:s a 1] [:s a 2] [:s a 3] [:s b 4] [:s b 5]]]
             (parse-pred-obj-list ":a 1,2,3;:b 4,5]" 0 g))))))

#?(:cljs (cljs.test/run-tests))
