(ns quoll.raphael.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [quoll.raphael.core :refer [skip-whitespace skip-to dot? newline?
                                        parse-iri-ref add-prefix new-generator]])
  (:import [clojure.lang ExceptionInfo]))

(deftest ws-test
  (testing "Tests if whitespace is skipped correctly."
    (is (= [3 \a] (skip-whitespace "   a b" 0)))
    (is (= [3 \a] (skip-whitespace "   a b" 2)))
    (is (= [3 \a] (skip-whitespace "   a b" 3)))
    (is (= [5 \b] (skip-whitespace "   a b" 4)))
    (is (= [-1 :eof] (skip-whitespace "   a b" 6)))))

(deftest skip-to-test
  (testing "Testing skipping whitespace to a character"
    (is (= [4 \space] (skip-to "   . a" 0 dot?)))
    (is (= [4 \space] (skip-to "   . a" 1 dot?)))
    (is (= [4 \space] (skip-to "   . a" 3 dot?)))
    (is (thrown? ExceptionInfo (skip-to "   . a" 4 dot?)))
    (is (= [4 \space] (skip-to "   \n a" 0 newline?)))
    (is (= [4 \space] (skip-to "   \n a" 1 newline?)))
    (is (= [4 \space] (skip-to "   \n a" 3 newline?)))
    (is (thrown? ExceptionInfo (skip-to "   \n a" 4 newline?)))))

(deftest iri-ref-test
  (testing "Parsing an IRI reference"
    (is (= [16 "http://ex.com/"]
           (parse-iri-ref "<http://ex.com/>" 0 \< nil)))
    (is (= [46 "http://example.com/path?query=x&y=2#fragment"]
           (parse-iri-ref "<http://example.com/path?query=x&y=2#fragment>" 0 \< nil)))
    (is (= [20 "http://ex.com/"]
           (parse-iri-ref "foo <http://ex.com/>" 4 \< nil)))
    (is (= [50 "http://example.com/path?query=x&y=2#fragment"]
           (parse-iri-ref "foo <http://example.com/path?query=x&y=2#fragment>" 4 \< nil)))
    (is (thrown? ExceptionInfo
                 (parse-iri-ref "<http://example.com/path query=x&y=2#fragment>" 0 \< nil)))
    (is (thrown? ExceptionInfo
                 (parse-iri-ref "<http://example.com/path?query=x&y=2#fragment>" 1 \h nil)))
    (is (thrown? ExceptionInfo
                 (parse-iri-ref "<http://example.com/path?query=x&y=2#fragment" 0 \< nil)))
    (let [g (-> (new-generator) (add-prefix :base "http://test.org/"))]
      (is (= [6 "path"] (parse-iri-ref "<path>" 0 \< nil)))
      (is (= [6 "http://test.org/path"] (parse-iri-ref "<path>" 0 \< g))))))
