(ns ^{:doc "Text functions, for building and parsing strings"
      :author "Paula Gearon"}
    quoll.text)

(defn string-builder
  "Creates a mutable string builder.
  init - optional initial string.
  return: an object that can be appended to."
  ([] (StringBuilder.))
  ([init] (StringBuilder. init)))

(defn append!
  "Appends data to a string builders.
  sb - the string builder.
  data - the data to append in string form.
  return: the string builder (may be ignored since it mutates)"
  [sb data]
  (.append sb data))

(defn last-char
  "Returns the last char appended to a string builder.
  Equivalent to `(last (str sb))` but uses direct calls.
  sb - the mutable string builder.
  return: the final char."
  [sb]
  #?(:clj (.charAt sb (dec (.length sb)))
     :cljs (let [s (str sb)] (nth s (dec (count sb))))))

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
