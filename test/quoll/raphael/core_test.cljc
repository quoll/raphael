(ns quoll.raphael.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [quoll.raphael.core :refer [skip-whitespace skip-to dot? newline?
                                        parse-iri-ref add-prefix new-generator parse-statement
                                        parse-local parse-prefixed-name
                                        ->QName]]
            [quoll.raphael.text :refer [char-at]])
  (:import [clojure.lang ExceptionInfo]))

(defn skip-whitespace' [s n] (skip-whitespace s n (char-at s n)))

(deftest ws-test
  (testing "Tests if whitespace is skipped correctly."
    (is (= [3 \a] (skip-whitespace' "   a b" 0)))
    (is (= [3 \a] (skip-whitespace' "   a b" 2)))
    (is (= [3 \a] (skip-whitespace' "   a b" 3)))
    (is (= [5 \b] (skip-whitespace' "   a b" 4)))
    (is (= [-1 :eof] (skip-whitespace' "   a b" 6)))))

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

(deftest iri-ref-test
  (testing "Parsing an IRI reference"
    (is (= [16 :eof "http://ex.com/"]
           (parse-iri-ref "<http://ex.com/>" 0 \< nil)))
    (is (= [46 :eof "http://example.com/path?query=x&y=2#fragment"]
           (parse-iri-ref "<http://example.com/path?query=x&y=2#fragment>" 0 \< nil)))
    (is (= [20 :eof "http://ex.com/"]
           (parse-iri-ref "foo <http://ex.com/>" 4 \< nil)))
    (is (= [50 :eof "http://example.com/path?query=x&y=2#fragment"]
           (parse-iri-ref "foo <http://example.com/path?query=x&y=2#fragment>" 4 \< nil)))
    (is (thrown? ExceptionInfo
                 (parse-iri-ref "<http://example.com/path query=x&y=2#fragment>" 0 \< nil)))
    (is (thrown? ExceptionInfo
                 (parse-iri-ref "<http://example.com/path?query=x&y=2#fragment>" 1 \h nil)))
    (is (thrown? ExceptionInfo
                 (parse-iri-ref "<http://example.com/path?query=x&y=2#fragment" 0 \< nil)))
    (let [g (-> (new-generator) (add-prefix :base "http://test.org/"))]
      (is (= [6 :eof  "path"] (parse-iri-ref "<path>" 0 \< nil)))
      (is (= [6 :eof "http://test.org/path"] (parse-iri-ref "<path>" 0 \< g))))))

(deftest non-triple-statement-test
  (testing "parsing statements that are not triples"
    (let [g (new-generator)
          [n1 c1 g1 t1] (parse-statement "BASE <http://test.org/>\n" 0 g)
          [n2 c2 g2 t2] (parse-statement "@base <http://test.com/>. \n" 0 g1)
          [n3 c3 g3 t3] (parse-statement "PREFIX test: <http://test.org/>\n" 0 g2)
          [n4 c4 g4 t4] (parse-statement "@prefix t2: <http://test.com/>. \n" 0 g3)
          [n5 c5 g5 t5] (parse-statement "@prefix t\ud800\udd49: <http://atticten.com/>. \n" 0 g4)
          [n6 c6 g6 t6] (parse-statement "@prefix \ud800\udd49x: <http://atticten.com/pre>. \n" 0 g5)
          [n7 c7 g7 t7] (parse-statement "@prefix \udb7f\udfffx: <http://unicode.com/limit>. \n" 0 g6)
          [n8 c8 g8 t8] (parse-statement "@prefix : <http://short.com/>. \n" 0 g7)
          ]
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
                   (parse-statement "@prefix \ud800\ud849x: <http://atticten.com/pre>. \n" 0 g5)))
      (is (thrown? ExceptionInfo
                   (parse-statement "@prefix \ud800x: <http://atticten.com/pre>. \n" 0 g5)))
      (is (thrown? ExceptionInfo
                   (parse-statement "@prefix \udd00\udd49x: <http://atticten.com/pre>. \n" 0 g5)))
      (is (thrown? ExceptionInfo
                   (parse-statement "@prefix \udb80\udc00x: <http://atticten.com/pre>. \n" 0 g5))))))

(deftest local-name-test
  (testing "Parsing the local portion of a prefixed name"
    (is (= [5 \space "alice"] (parse-local "alice " 0)))
    (is (= [6 \space "alice"] (parse-local " alice " 1)))
    (is (= [5 \. "alice"] (parse-local "alice. " 0)))
    (is (= [7 \space "alice.b"] (parse-local "alice.b " 0)))
    (is (= [5 \. "alice"] (parse-local "alice. " 0)))
    (is (= [9 \space "alice%20b"] (parse-local "alice%20b " 0)))
    (is (= [8 \space "alice\\!b"] (parse-local "alice\\!b " 0)))
    (is (= [9 \space "alice\\%ba"] (parse-local "alice\\%ba " 0)))
    (is (thrown? ExceptionInfo (parse-local "alice%%b " 0)))
    (is (thrown? ExceptionInfo (parse-local "alice%bg " 0)))
    (is (thrown? ExceptionInfo (parse-local "alice\\bg " 0)))))

(deftest prefixed-name-test
  (testing "Parsing prefixed names"
    (let [g (-> (new-generator)
                (add-prefix "ex" "http://ex.com/")
                (add-prefix "" "http://test.org/")
                (add-prefix "a-b-c" "http://t1.org/")
                (add-prefix "a_b" "http://t2.org/")
                (add-prefix "a.b" "http://t3.org/"))]
      (is (= [6 \space (->QName "ex" "cat" "http://ex.com/cat")]
             (parse-prefixed-name "ex:cat " 0 \e g)))
      (is (= [6 \. (->QName "ex" "cat" "http://ex.com/cat")]
             (parse-prefixed-name "ex:cat. " 0 \e g)))
      (is (= [4 \space (->QName "" "cat" "http://test.org/cat")]
             (parse-prefixed-name ":cat " 0 \: g)))
      (is (= [4 \. (->QName "" "cat" "http://test.org/cat")]
             (parse-prefixed-name ":cat. " 0 \: g)))
      (is (= [9 \space (->QName "a-b-c" "cat" "http://t1.org/cat")]
             (parse-prefixed-name "a-b-c:cat " 0 \a g)))
      (is (= [7 \space (->QName "a_b" "cat" "http://t2.org/cat")]
             (parse-prefixed-name "a_b:cat " 0 \a g)))
      (is (= [7 \space (->QName "a.b" "cat" "http://t3.org/cat")]
             (parse-prefixed-name "a.b:cat " 0 \a g)))
      (is (thrown? ExceptionInfo (parse-prefixed-name "_b:cat " 0 \_ g)))
      (is (thrown? ExceptionInfo (parse-prefixed-name ".b:cat " 0 \. g)))
      (is (thrown? ExceptionInfo (parse-prefixed-name "-b:cat " 0 \- g))))))
