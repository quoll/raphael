(ns ^{:doc "Macro namespace"
      :author "Paula Gearon"}
    quoll.raphael.m
  (:require [clojure.string :as str]))


(def ^:const WIDTH 80)

(defn line-at
  "Returns a line of text that is no more that WIDTH characters. Used for user output."
  [s n]
  (let [line (subs s (max n 0) (min (+ n WIDTH) (count s)))
        nl (str/index-of line \newline)]
    (if nl (subs line 0 nl) line)))

(defmacro throw-unex
  "Convenience macro to print a message, the offset and the current line.
  msg - the message to print. The line will be appended.
  s - the string that was being parsed.
  n - the offset in the string for the data that caused the error."
  [loc msg s n]
  `(throw (ex-info (str ~msg (line-at ~s ~n))
                   (let  [l# (deref ~loc)]
                     {:line (nth l# 0) :offset (- ~n (nth l# 1))}))))
