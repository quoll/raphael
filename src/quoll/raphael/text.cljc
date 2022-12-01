(ns ^{:doc "Text functions, for building and parsing strings"
      :author "Paula Gearon"}
    quoll.raphael.text)

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
  #?(:clj (let [len (.length sb)] (when (> len 0) (.charAt sb (dec len))))
     :cljs (let [s (str sb)
                 len (count sb)]
             (when (> len 0)
               (nth s (dec len))))))

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
