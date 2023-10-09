# quoll/raphael

A Clojure/ClojureScript library for parsing strings containing the [Terse Triples Language: Turtle](https://www.w3.org/TR/turtle/).

This parser is in pure Clojure for portability.
It also avoids parser tools, in an effort to parse large documents quickly.

## Usage

### Leiningen/Boot
```clojure
[org.clojars.quoll/raphael "0.3.2"]
```

### Clojure CLI/deps.edn
```clojure
org.clojars.quoll/raphael {:mvn/version "0.3.2"}
```

After bringing in the project, just use the `parse` function on a string or a Java reader.
```clojure
(:require '[quoll.raphael.core :refer [parse]])

(parse (slurp "data.ttl"))
(parse (clojure.java.io/reader "data.ttl"))  ;; Clojure only
```

This returns an in-memory structure with 3 fields:
 - **:base** (optional) A string with the base IRI of the document at the time of exit.
 - **:namespaces** A map of prefix strings to strings containing namespace IRIs.
 - **:triples** A vector of triples. Triples are a 3 element vector.

The output could be streamed instead of going to a structure, but this is not publicly accessible. Please ask if this is desired.

## Pluggable

This parser uses a pluggable `Generator` protocol so that custom objects can be
instantiated for specific applications. A generator is passed as an optional argument
to the `parse` function:
```clojure
(:require '[quoll.raphael.core :refer [parse]])

(parse (slurp "data.ttl") my-generator)
```

The Generator protocol defines the following functions:

 - `add-prefix` Maps a prefix string to a namespace IRI. This is used for triples that use qualified names,
 - `add-base` Sets the base IRI for the document. All relative IRIs are relative to this base.
 - `iri-for` Returns the IRI that a prefix has been mapped to via `add-prefix`.
 - `get-namespaces` Returns a map of all prefix strings to strings of the IRIs they were mapped to via `add-prefix`.
 - `get-base` Returns the base IRI for the document.
 - `new-node` Creates a blank node, optionally for a label. If the same label is presented more than once, the same node should be returned.
 - `new-qname` Creates an using a Qualified Name prefix and local name pair. The prefix must refer to a known namespace.
 - `new-iri` Creates an IRI object from a string representation of the IRI. If relative, then the base will be read.
 - `new-literal` Creates a literal object, optionally with a type IRI object.
 - `new-lang-string` Creates a string object, with a provided language code.
 - `rdf-type` Returns a constant IRI value for `rdf:type`.
 - `rdf-first` Returns a constant IRI value for `rdf:first`.
 - `rdf-rest` Returns a constant IRI value for `rdf:rest`.
 - `rdf-nil` Returns a constant IRI value for `rdf:nil`.

These functions should have trivial implementations. They are used to adapt the return types of the parser to objects that are compatible with the calling system.

If no Generator is provided, then a default generator is used. The default generator created internal objects which all support the `str` function:
 - `quoll.raphael.core/IriRef` - an IRI reference. This will print as a QName if it was read that way. Use `as-iri-string` to get the full IRI, even if it can be represented as a QName.
 - `quoll.raphael.core/BlankNode` - A blank node.
 - `quoll.raphael.core/Literal` - A literal containing a string representation of the literal, and either a language code, or a datatype IRI.

Simple literals will typically be returned as native data types:
 - String
 - Long Integer
 - Double
 - Boolean


## Principles

Programs written in Clojure will typically use parsers that have already been built in host languages, most often Java.
This is to avoid duplication of effort when a parser already exists, and also because parsers perform very well
when they are built on stateful operations, which are provided in most imperative languages. The lack of native
support is not an issue unless a system is trying to provide identical functionality between multiple platforms,
such as Java and JavaScript. However, Clojure parsers will typically be slower than the equivalent parser in an
imperative host language like Java.

Parsers themselves are typically created by Parser Generators, and these  are configured with languages such as EBNF.
However, the flexibility of these systems comes with a performance cost. This should not matter in most cases
but Turtle files may be MB or GB in size, so poor performance is noticeable. Coupled with the performance concerns
of a functional language, this suggests that a parser for large Turtle files would be difficult to write efficiently
in Clojure.

One possibile mitigation would be to use various unusual patterns in Clojure to attempt to get better performance.
This is the approach that _Raphael_ has taken.

### Approach
Most of the approach that this parser uses comes out of experience in processing data into triples in other systems,
especially Asami.

#### Building Triples
Prior experience in Clojure systems that build sequences of triples showed that functions that return blocks of
triples needed to be concattenated together. Whether this was done via `concat` or `insert`, the accumulation of
vectors as return values from functions proved to have a performance cost.

Shifting to a single _transitive_ vector and accumulating that triples into this object led to much better performance.

#### Mutation
The first use of a _transitive_ accumulator stored it in a _Atom_, which are threadsafe mutable values. However,
these parsers do not communicate between multiple threads, so a less safe type of mutation can be adopted.
Clojure's _Volatile_ values are similar to _Atoms_, but without the thread safety guarantees. On the JVM they
are still built with Java `volatile` values, which force memory synchronization. This is an unnecessary cost when the
values are isolated to the stack of a single-threaded process, but without providing host-based extentions then this
can't really be avoided. (Perhaps a hosted array could be used as a mutable object -
I have yet to test and compare this to a _volatile_).

#### Stack Values
My first attempt at using a _transitive_ was a lot faster, but it also stored the transitive in global _Atoms_ and
_Volatiles_. While a single stack may not use multiple threads, it is still possible for multiple threads to
call the parser. Fortunately, Clojure has _dynamic vars_ which can be bound to a unique value for each thread,
and this proved to work well..

However, these thread-local values turn out to be extremely slow to access compared to normal values (over 10 times slower).
This adds up over millions of lines. But how else can values be accessed by functions at every level of the parser,
given that we don't want global state, nor do we want thread-local state? It has to be achieved through the orignal
mechanism of passing values up and down the stack. This has numerous benefits:
 - It's faster than other approaches.
 - It is inherently thread safe.
 - It allows for referential transparency, making code easier to define, and test in isolation.

The disadvantage is that it requires extra parameters and return values throughout the entire codebase. This makes for
messy and boilerplate code, but those issues can be mitigated by adopting a consistent pattern for arguments passedi
in and out.

A potential issue was in the multiple return types. Each time data is to be returned, it needs to be packaged into a
vector, and then the called needs to unpack this data. Given the multiple function calls necessary to construct a vector
I tried several approaches including hosted arrays, only to find that vector packing and destructuring was the fastest
mechanism, only taking < 10 nanoseconds each time.

#### Lookahead
Right now the only data source supported is a string. That's not as significant a limitation as it once was, because
`slurp`ing GB files into memory is withing the capabilities of most modern computers.

However, I would eventually like the capability to stream things that are much larger. To that end the parser avoids
using "Lookahead" for most operations. This means that it could work just as well on a stream as on a string.

The current exceptions to this are:
 - **Numbers**: These are currently pulled of the head of the string with a regex. This can be replaced with a short function.
 - **Language codes**: Like Numbers, these are pulled in via a regex, and can be reimplemented with a function.
 - **true**/**false**: These are more awkward because they could just as easily be the beginning of a Qualified Name
                       with a prefix (e.g. `true-ish:data`). Only when they finish with a non-prefix character can
                       it be guaranteed that they are a boolean. This required a change to the code to avoid backtracking.
 - **PREFIX**/**BASE**: Like boolean literals, these could be the beginning of Qualified Names.

The first 2 are minor. The last 2 need a little thought. The simple answer is to use a PushbackReader (as included on the JVM)
but this is less available on JavaScript.


## License

Copyright © 2023 Paula Gearon

Distributed under the Eclipse Public License version 2.0.
