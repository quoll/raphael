(ns quoll.raphael.reader
  "Described a reader interface used for wrapping java.io.Reader
   and similar objects"
  {:author "Paula Gearon"}
  (:require [quoll.raphael.text :as text])
  #?(:clj (:import [java.io Reader StringReader])))

(defprotocol PositionReader
  (get-char! [this] "Returns the next char to be read. Increments location.")
  (position [this] "Returns the current character location"))

#?(:clj
   (defrecord StringReaderWrapper [^Reader rdr vpos]
     PositionReader
     (get-char!
       [this]
       (let [c (.read ^Reader rdr)]
         (if (not= -1 c)
           (do
             (vswap! vpos inc)
             (char c))
           :eof)))

     (position [this] @vpos)))

(defrecord StringWrapper [s vpos len]
  PositionReader
  (get-char!
    [this]
    (let [pos @vpos]
      (if (< pos len)
        (do
          (vswap! vpos inc)
          (.charAt #?(:clj ^String s :cljs s) pos))
        :eof)))

  (position [this] @vpos))

(defn position-reader
  [source]
  (cond
    (string? source) (->StringWrapper source (volatile! 0) (count source))
    #?@(:clj [(instance? Reader source) (StringReaderWrapper. source (volatile! 0))])
    :default (throw (ex-info (str "Unknown source type: " (type source)) {:source source}))))

(defn readn!
  "Reads the first n chars from a reader, returning the chars in a string."
  [^Reader r n]
  (let [sb (text/string-builder)]
    (dotimes [cn n]
      (let [c (get-char! r)]
        (when-not (= :eof c) (text/append! sb c))))
    (str sb)))

(defn readn-line!
  "Reads the first n chars from a reader up to the end of a line, returning the chars in a string."
  [^Reader r n]
  (let [sb (text/string-builder)]
    (loop [n' n]
      (let [c (get-char! r)]
        (if (or (= :eof c) (zero? n') (= \newline c))
          (str sb)
          (do
            (text/append! sb c)
            (recur (dec n'))))))))

(defn dropn!
  "Reads the first n chars from a reader, dropping them."
  [^Reader r n]
  (loop [n n c nil]
    (if (and (> n 0) (not= c :eof))
      (recur (inc n) (get-char! r)))))
