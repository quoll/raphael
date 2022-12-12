(ns ^{:doc "Text functions, for building and parsing strings"
      :author "Paula Gearon"}
    quoll.raphael.text
  #?(:cljs (:import [goog.string StringBuffer])))

(defn string-builder
  "Creates a mutable string builder.
  init - optional initial string.
  return: an object that can be appended to."
  ([] #?(:clj (StringBuilder.) :cljs (StringBuffer.)))
  ([^String init] #?(:clj (StringBuilder. init) :cljs (StringBuffer.))))

(defn append!
  "Appends data to a string builders.
  sb - the string builder.
  data - the data to append in string form.
  return: the string builder (may be ignored since it mutates)"
  [^StringBuilder sb data]
  (.append sb data))

(defn last-char
  "Returns the last char appended to a string builder.
  Equivalent to `(last (str sb))` but uses direct calls.
  sb - the mutable string builder.
  return: the final char."
  [^StringBuilder sb]
  #?(:clj (let [len (.length sb)] (when (> len 0) (.charAt sb (dec len))))
     :cljs (let [s (str sb)
                 len (.-length sb)]
             (when (> len 0)
               (.charAt s (dec len))))))

(defn last-char-str
  "Returns the last char of a str.
  Equivalent to `(last s)` but in constant time.
  s - The string.
  return: the final char."
  [^String s]
  #?(:clj (let [len (.length s)] (when (> len 0) (.charAt s (dec len))))
     :cljs (let [len (.-length s)]
             (when (> len 0)
               (.charAt s (dec len))))))

(defn char-at
  "Returns the character at a given position in the string,
  or :eof at the end of the string."
  [^String s ^long n]
  #?(:clj (if (< n (.length s)) (.charAt s n) :eof)
     :cljs (if (< n (.-length s)) (.charAt s n) :eof)))

(defn parse-hex
  "Parses a hexadecimal string into a long"
  [s]
  #?(:clj (Long/parseLong s 16)
     :cljs (js/parseInt s 16)))

(defn ssubs
  "A safe substring function. The same as a 3 argument `subs` but will not run out of range."
  [s start end]
  (subs s start (min end (count s))))

(defn high-surrogate?
  "Tests if a character is both a high surrogate (0xD800 <= c <= 0xDBFF)
  and also in range (0xD800 <= c <= 0xDB7F) which matches the character range 0x10000 to 0xEFFFF"
  [c]
  (<= 0xD800 (long c) 0xDB7F))

(defn low-surrogate?
  "Tests if a character is a low surrogate (0xDC00 <= c <= 0xDFFF)
  which matches the character range 0x10000 to 0xEFFFF"
  [c]
  (<= 0xDC00 (long c) 0xDFFF))

(defn surrogates
  "Converts a long value to a high/log surrogate pair."
  [u]
  [(+ 0xd7c0 (unsigned-bit-shift-right u 10)) (+ 0xDC00 (bit-and u 0x3FF))])
