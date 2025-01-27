(ns quoll.raphael.doc-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]
            [quoll.raphael.core :refer [parse]]
            #?(:clj [quoll.rdf :as rdf]
               :cljs [quoll.rdf :as rdf :refer [IRI]]))
  #?(:clj (:import [clojure.lang ExceptionInfo]
                   [quoll.rdf IRI])))

(def document1
"@base <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix rel: <http://www.perceive.net/schemas/relationship/> .

<#green-goblin>
    rel:enemyOf <#spiderman> ;
    a foaf:Person ;    # in the context of the Marvel universe
    foaf:name \"Green Goblin\" .

<#spiderman>
    rel:enemyOf <#green-goblin> ;
    a foaf:Person ;
    foaf:name \"Spiderman\", \"Человек-паук\"@ru .")

(def document9
"# A triple with all absolute IRIs
<http://one.example/subject1> <http://one.example/predicate1> <http://one.example/object1> .

@base <http://one.example/> .
<subject2> <predicate2> <object2> .     # relative IRIs, e.g. http://one.example/subject2

BASE <http://one.example/>
<subject2> <predicate2> <object2> .     # relative IRIs, e.g. http://one.example/subject2

@prefix p: <http://two.example/> .
p:subject3 p:predicate3 p:object3 .     # prefixed name, e.g. http://two.example/subject3

PREFIX p: <http://two.example/>
p:subject3 p:predicate3 p:object3 .     # prefixed name, e.g. http://two.example/subject3

@prefix p: <path/> .                    # prefix p: now stands for http://one.example/path/
p:subject4 p:predicate4 p:object4 .     # prefixed name, e.g. http://one.example/path/subject4

@prefix : <http://another.example/> .    # empty prefix
:subject5 :predicate5 :object5 .        # prefixed name, e.g. http://another.example/subject5

:subject6 a :subject7 .                 # same as :subject6 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> :subject7 .

<http://伝言.example/?user=أكرم&amp;channel=R%26D> a :subject8 . # a multi-script subject IRI .")

(def document11
"@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix show: <http://example.org/vocab/show/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

show:218 rdfs:label \"That Seventies Show\"^^xsd:string .            # literal with XML Schema string datatype
show:218 rdfs:label \"That Seventies Show\"^^<http://www.w3.org/2001/XMLSchema#string> . # same as above
show:218 rdfs:label \"That Seventies Show\" .                                            # same again
show:218 show:localName \"That Seventies Show\"@en .                 # literal with a language tag
show:218 show:localName 'Cette Série des Années Soixante-dix'@fr . # literal delimited by single quote
show:218 show:localName \"Cette Série des Années Septante\"@fr-be .  # literal with a region subtag
show:218 show:blurb '''This is a multi-line                        # literal with embedded new lines and quotes
literal with many quotes (\"\"\"\"\")
and up to two sequential apostrophes ('').''' .")

(def document12
"@prefix : <http://example.org/elements> .
<http://en.wikipedia.org/wiki/Helium>
    :atomicNumber 2 ;               # xsd:integer
    :atomicMass 4.002602 ;          # xsd:decimal
    :specificGravity 1.663E-4 .     # xsd:double  ")

(def document13
"@prefix : <http://example.org/stats> .
<http://somecountry.example/census2007>
    :isLandlocked false .           # xsd:boolean")

(def document14
"@prefix foaf: <http://xmlns.com/foaf/0.1/> .

_:alice foaf:knows _:bob .
_:bob foaf:knows _:alice .")

(def document15
"@prefix foaf: <http://xmlns.com/foaf/0.1/> .

# Someone knows someone else, who has the name \"Bob\".
[] foaf:knows [ foaf:name \"Bob\" ] .")

(def document16
"@prefix foaf: <http://xmlns.com/foaf/0.1/> .

[ foaf:name \"Alice\" ] foaf:knows [
    foaf:name \"Bob\" ;
    foaf:knows [
        foaf:name \"Eve\" ] ;
    foaf:mbox <bob@example.com> ] .")

(def document17
"_:a <http://xmlns.com/foaf/0.1/name> \"Alice\" .
_:a <http://xmlns.com/foaf/0.1/knows> _:b .
_:b <http://xmlns.com/foaf/0.1/name> \"Bob\" .
_:b <http://xmlns.com/foaf/0.1/knows> _:c .
_:c <http://xmlns.com/foaf/0.1/name> \"Eve\" .
_:b <http://xmlns.com/foaf/0.1/mbox> <bob@example.com> .")

(def document19
"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix dc: <http://purl.org/dc/elements/1.1/> .
@prefix ex: <http://example.org/stuff/1.0/> .

<http://www.w3.org/TR/rdf-syntax-grammar>
  dc:title \"RDF/XML Syntax Specification (Revised)\" ;
  ex:editor [
    ex:fullname \"Dave Beckett\";
    ex:homePage <http://purl.org/net/dajobe/>
  ] .")

(def document20
"PREFIX : <http://example.org/stuff/1.0/>
:a :b ( \"apple\" \"banana\" ) .")


(def document21
"@prefix : <http://example.org/stuff/1.0/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
:a :b
  [ rdf:first \"apple\";
    rdf:rest [ rdf:first \"banana\";
               rdf:rest rdf:nil ]
  ] .")


(def document22
"@prefix : <http://example.org/stuff/1.0/> .

:a :b \"The first line\\nThe second line\\n  more\" .

:a :b \"\"\"The first line
The second line
  more\"\"\" .")

(def document23
"@prefix : <http://example.org/stuff/1.0/> .
(1 2.0 3E1) :p \"w\" .")

(def document23a
"@prefix : <http://example.org/stuff/1.0/> .
(3E1 2.0 1.) :p \"w\" .")

(def document24
"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
    _:b0  rdf:rest   _:b1;
          rdf:first  1.
    _:b1  rdf:first  2.0;
          rdf:rest   _:b2 .
    _:b2  rdf:first  3E1;
          rdf:rest   rdf:nil.
    _:b0  :p         \"w\".")

(def document25
"PREFIX : <http://example.org/stuff/1.0/>
(1 [:p :q] ( 2 ) ) :p2 :q2 .")

(def document26
"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
    _:b0  rdf:first  1 ;
          rdf:rest   _:b1 .
    _:b1  rdf:first  _:b2 .
    _:b2  :p         :q .
    _:b1  rdf:rest   _:b3 .
    _:b3  rdf:first  _:b4 .
    _:b4  rdf:first  2 ;
          rdf:rest   rdf:nil .
    _:b3  rdf:rest   rdf:nil .")

(def document27
  "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ent: <tag:acme.com/initrode/pd/mm/id/e/> .
@prefix attr: <tag:acme.com/initrode/pd/mm/id/a/> .

ent:1efd782a-47b7-6790-8487-440cb3ab6e31 attr:paths \"0101, 1010\";
                                         attr:sop_ref_id 12345;
                                         attr:build \"\";
                                         attr:vendor \"ACME\";
                                         attr:cat 1;
                                         attr:project_code 8675309;
                                         attr:number \"C2187\";
                                         attr:pattern_type \"p1x1\";
                                         attr:elastic \"no\".")

(defn simple
  [e]
  (cond
    (instance? IRI e) (if-let [local (:local e)]
                        (let [p (:prefix e)]
                          (if (and p (seq (name p)))
                            (keyword (name p) (:local e))
                            (keyword (:local e))) )
                        (str e))
    (number? e) e
    (boolean? e) e
    :default (str e)))

(defn simplify
  [triples]
  (mapv #(mapv simple %) triples))

(deftest document1-test
  (testing "Parsing of example1 document"
    (let [ex "http://example.org/"
          gg (str "<" ex "#green-goblin>")
          sm (str "<" ex "#spiderman>")
          {:keys [base namespaces triples]} (parse document1)]
      (is (= base "http://example.org/"))
      (is (= namespaces {"rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                         "rdfs" "http://www.w3.org/2000/01/rdf-schema#"
                         "foaf" "http://xmlns.com/foaf/0.1/"
                         "rel" "http://www.perceive.net/schemas/relationship/"}))
      (is (= (simplify triples)
             [[gg :rel/enemyOf sm]
              [gg :rdf/type :foaf/Person]
              [gg :foaf/name "Green Goblin"]
              [sm :rel/enemyOf gg]
              [sm :rdf/type :foaf/Person]
              [sm :foaf/name "Spiderman"]
              [sm :foaf/name "\"Человек-паук\"@ru"]])))))


(deftest document9-test
  (testing "Parsing of example9 document"
    (let [ex "http://one.example/"
          {:keys [base namespaces triples]} (parse document9)]
      (is (= base "http://one.example/"))
      (is (= namespaces {"p" "http://one.example/path/"
                         "" "http://another.example/"}))
      (is (= (simplify triples)
             [["<http://one.example/subject1>" "<http://one.example/predicate1>" "<http://one.example/object1>"]
              ["<http://one.example/subject2>" "<http://one.example/predicate2>" "<http://one.example/object2>"]
              ["<http://one.example/subject2>" "<http://one.example/predicate2>" "<http://one.example/object2>"]
              [:p/subject3 :p/predicate3 :p/object3]
              [:p/subject3 :p/predicate3 :p/object3]
              [:p/subject4 :p/predicate4 :p/object4]
              [:subject5 :predicate5 :object5]
              [:subject6 :rdf/type :subject7]
              ["<http://伝言.example/?user=أكرم&amp;channel=R%26D>" :rdf/type :subject8]])))))


(deftest document11-test
  (testing "Parsing of example11 document"
    (let [ex "http://one.example/"
          {:keys [base namespaces triples]} (parse document11)
          show218 (keyword "show" "218")
          tstrings (take 3 (map #(nth % 2) triples))]
      (is (nil? base))
      (is (= namespaces {"rdfs" "http://www.w3.org/2000/01/rdf-schema#"
                         "show" "http://example.org/vocab/show/"
                         "xsd" "http://www.w3.org/2001/XMLSchema#"}))
      (= [(rdf/iri "http://www.w3.org/2001/XMLSchema#string")
          (rdf/iri "http://www.w3.org/2001/XMLSchema#string" "xsd" "string")
          nil] (map :datatype tstrings))
      (is (= (simplify triples)
             [[show218 :rdfs/label "\"That Seventies Show\""]
              [show218 :rdfs/label "\"That Seventies Show\""]
              [show218 :rdfs/label "That Seventies Show"]
              [show218 :show/localName "\"That Seventies Show\"@en"]
              [show218 :show/localName "\"Cette Série des Années Soixante-dix\"@fr"]
              [show218 :show/localName "\"Cette Série des Années Septante\"@fr-be"]
              [show218 :show/blurb "This is a multi-line                        # literal with embedded new lines and quotes
literal with many quotes (\"\"\"\"\")
and up to two sequential apostrophes ('')."]])))))


(deftest document12-test
  (testing "Parsing of example12 document"
    (let [{:keys [base namespaces triples]} (parse document12)]
      (is (nil? base))
      (is (= namespaces {"" "http://example.org/elements"}))
      (is (= (simplify triples)
             [["<http://en.wikipedia.org/wiki/Helium>" :atomicNumber 2]
              ["<http://en.wikipedia.org/wiki/Helium>" :atomicMass 4.002602]
              ["<http://en.wikipedia.org/wiki/Helium>" :specificGravity 1.663E-4]]))
      (is (every? #(str/starts-with? (:iri (nth % 1)) "http://example.org/elements")
                  triples)))))


(deftest document13-test
  (testing "Parsing of example13 document"
    (let [{:keys [base namespaces triples]} (parse document13)]
      (is (nil? base))
      (is (= namespaces {"" "http://example.org/stats"}))
      (is (= (simplify triples)
             [["<http://somecountry.example/census2007>" :isLandlocked false]])))))


(deftest document14-test
  (testing "Parsing of example14 document"
    (let [{:keys [base namespaces triples]} (parse document14)]
      (is (nil? base))
      (is (= namespaces {"foaf" "http://xmlns.com/foaf/0.1/"}))
      (is (= (simplify triples)
             [["_:b0" :foaf/knows "_:b1"]
              ["_:b1" :foaf/knows "_:b0"]])))))


(deftest document15-test
  (testing "Parsing of example15 document"
    (let [{:keys [base namespaces triples]} (parse document15)]
      (is (nil? base))
      (is (= namespaces {"foaf" "http://xmlns.com/foaf/0.1/"}))
      (is (= (simplify triples)
             [["_:b1" :foaf/name "Bob"]
              ["_:b0" :foaf/knows "_:b1"]])))))


(deftest document16-test
  (testing "Parsing of example16 document"
    (let [{:keys [base namespaces triples]} (parse document16)]
      (is (nil? base))
      (is (= namespaces {"foaf" "http://xmlns.com/foaf/0.1/"}))
      (is (= (simplify triples)
             [["_:b0" :foaf/name "Alice"]
              ["_:b1" :foaf/name "Bob"]
              ["_:b2" :foaf/name "Eve"]
              ["_:b1" :foaf/knows "_:b2"]
              ["_:b1" :foaf/mbox "<bob@example.com>"]
              ["_:b0" :foaf/knows "_:b1"]])))))


(deftest document17-test
  (testing "Parsing of example17 document"
    (let [{:keys [base namespaces triples]} (parse document17)]
      (is (nil? base))
      (is (empty? namespaces))
      (is (= (simplify triples)
             [["_:b0" "<http://xmlns.com/foaf/0.1/name>" "Alice"]
              ["_:b0" "<http://xmlns.com/foaf/0.1/knows>" "_:b1"]
              ["_:b1" "<http://xmlns.com/foaf/0.1/name>" "Bob"]
              ["_:b1" "<http://xmlns.com/foaf/0.1/knows>" "_:b2"]
              ["_:b2" "<http://xmlns.com/foaf/0.1/name>" "Eve"]
              ["_:b1" "<http://xmlns.com/foaf/0.1/mbox>" "<bob@example.com>"]])))))


(deftest document19-test
  (testing "Parsing of example19 document"
    (let [{:keys [base namespaces triples]} (parse document19)]
      (is (nil? base))
      (is (= namespaces {"rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                         "dc" "http://purl.org/dc/elements/1.1/"
                         "ex" "http://example.org/stuff/1.0/"}))
      (is (= (simplify triples)
             [["<http://www.w3.org/TR/rdf-syntax-grammar>" :dc/title "RDF/XML Syntax Specification (Revised)"]
              ["_:b0" :ex/fullname "Dave Beckett"]
              ["_:b0" :ex/homePage "<http://purl.org/net/dajobe/>"]
              ["<http://www.w3.org/TR/rdf-syntax-grammar>" :ex/editor "_:b0"]])))))


(deftest document20-test
  (testing "Parsing of example20 document"
    (let [{:keys [base namespaces triples]} (parse document20)]
      (is (nil? base))
      (is (= namespaces {"" "http://example.org/stuff/1.0/"}))
      (is (= (simplify triples)
             [["_:b0" :rdf/first "apple"]
              ["_:b0" :rdf/rest "_:b1"]
              ["_:b1" :rdf/first "banana"]
              ["_:b1" :rdf/rest :rdf/nil]
              [:a :b "_:b0"]])))))


(deftest document21-test
  (testing "Parsing of example21 document"
    (let [{:keys [base namespaces triples]} (parse document21)]
      (is (nil? base))
      (is (= namespaces {"" "http://example.org/stuff/1.0/"
                         "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"}))
      (is (= (simplify triples)
             [["_:b0" :rdf/first "apple"]
              ["_:b1" :rdf/first "banana"]
              ["_:b1" :rdf/rest :rdf/nil]
              ["_:b0" :rdf/rest "_:b1"]
              [:a :b "_:b0"]])))))


(deftest document22-test
  (testing "Parsing of example22 document"
    (let [{:keys [base namespaces triples]} (parse document22)]
      (is (nil? base))
      (is (= namespaces {"" "http://example.org/stuff/1.0/"}))
      (is (= (simplify triples)
             [[:a :b "The first line\nThe second line\n  more"]
              [:a :b "The first line\nThe second line\n  more"]])))))


(deftest document23-test
  (testing "Parsing of example23 document"
    (let [{:keys [base namespaces triples]} (parse document23)]
      (is (nil? base))
      (is (= namespaces {"" "http://example.org/stuff/1.0/"}))
      (is (= (simplify triples)
             [["_:b0" :rdf/first 1]
              ["_:b0" :rdf/rest "_:b1"]
              ["_:b1" :rdf/first 2.0]
              ["_:b1" :rdf/rest "_:b2"]
              ["_:b2" :rdf/first 30.0]
              ["_:b2" :rdf/rest :rdf/nil]
              ["_:b0" :p "w"]])))))


(deftest document23a-test
  (testing "Parsing of example23a document"
    (let [{:keys [base namespaces triples]} (parse document23a)]
      (is (nil? base))
      (is (= namespaces {"" "http://example.org/stuff/1.0/"}))
      (is (= (simplify triples)
             [["_:b0" :rdf/first 30.0]
              ["_:b0" :rdf/rest "_:b1"]
              ["_:b1" :rdf/first 2.0]
              ["_:b1" :rdf/rest "_:b2"]
              ["_:b2" :rdf/first 1.0]
              ["_:b2" :rdf/rest :rdf/nil]
              ["_:b0" :p "w"]])))))


(deftest document24-test
  (testing "Parsing of example24 document"
    (let [{:keys [base namespaces triples]} (parse document24)]
      (is (nil? base))
      (is (= namespaces {"rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"}))
      (is (= (simplify triples)
             [["_:b0" :rdf/rest "_:b1"]
              ["_:b0" :rdf/first 1]
              ["_:b1" :rdf/first 2.0]
              ["_:b1" :rdf/rest "_:b2"]
              ["_:b2" :rdf/first 30.0]
              ["_:b2" :rdf/rest :rdf/nil]
              ["_:b0" :p "w"]])))))


(deftest document25-test
  (testing "Parsing of example25 document"
    (let [{:keys [base namespaces triples]} (parse document25)]
      (is (nil? base))
      (is (= namespaces {"" "http://example.org/stuff/1.0/"}))
      (is (= (simplify triples)
             [["_:b0" :rdf/first 1]
              ["_:b0" :rdf/rest "_:b1"]
              ["_:b2" :p :q]
              ["_:b1" :rdf/first "_:b2"]
              ["_:b1" :rdf/rest "_:b3"]
              ["_:b4" :rdf/first 2]
              ["_:b4" :rdf/rest :rdf/nil]
              ["_:b3" :rdf/first "_:b4"]
              ["_:b3" :rdf/rest :rdf/nil]
              ["_:b0" :p2 :q2]
              ])))))


(deftest document26-test
  (testing "Parsing of example26 document"
    (let [{:keys [base namespaces triples]} (parse document26)]
      (is (nil? base))
      (is (= namespaces {"rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"}))
      (is (= (simplify triples)
             [["_:b0" :rdf/first 1]
              ["_:b0" :rdf/rest "_:b1"]
              ["_:b1" :rdf/first "_:b2"]
              ["_:b2" :p :q]
              ["_:b1" :rdf/rest "_:b3"]
              ["_:b3" :rdf/first "_:b4"]
              ["_:b4" :rdf/first 2]
              ["_:b4" :rdf/rest :rdf/nil]
              ["_:b3" :rdf/rest :rdf/nil]])))))


(deftest document27-test
  (testing "Parsing of example27 document"
    (let [{:keys [base namespaces triples]} (parse document27)
          main-node (keyword "ent" "1efd782a-47b7-6790-8487-440cb3ab6e31")]
      (is (nil? base))
      (is (= namespaces {"rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                         "rdfs" "http://www.w3.org/2000/01/rdf-schema#"
                         "ent" "tag:acme.com/initrode/pd/mm/id/e/"
                         "attr" "tag:acme.com/initrode/pd/mm/id/a/"}))
      (is (= (simplify triples)
             [[main-node :attr/paths "0101, 1010"]
              [main-node :attr/sop_ref_id 12345]
              [main-node :attr/build ""]
              [main-node :attr/vendor "ACME"]
              [main-node :attr/cat 1]
              [main-node :attr/project_code 8675309]
              [main-node :attr/number "C2187"]
              [main-node :attr/pattern_type "p1x1"]
              [main-node :attr/elastic "no"]])))))


#?(:cljs (cljs.test/run-tests))
