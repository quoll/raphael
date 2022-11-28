(ns quoll.raphael.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [quoll.raphael.core :refer [skip-whitespace skip-to dot? newline?]])
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
