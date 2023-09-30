(ns ^{:doc "Macro namespace"
      :author "Paula Gearon"}
    quoll.raphael.m
  (:require [clojure.string :as str]
            [quoll.raphael.reader :as reader]))


(def ^:const WIDTH 80)

(defn line-at
  "Returns a line of text that is no more that WIDTH characters. Used for user output."
  [r]
  (let [line (reader/readn! r WIDTH)
        nl (str/index-of line \newline)]
    (if nl (subs line 0 nl) line)))

(defmacro throw-unex
  "Convenience macro to print a message, the offset and the current line.
  msg - the message to print. The line will be appended.
  r - the reader that was being parsed from.
  s - the text already parsed that started the cause of the error."
  [loc msg r s]
  `(throw (ex-info (str ~msg ~s (line-at ~r))
                   (let  [l# (deref ~loc)]
                     {:line (nth l# 0) :offset (- (quoll.raphael.reader/position ~r) (nth l# 1))}))))
