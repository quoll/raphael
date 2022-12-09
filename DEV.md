## Usage

Invoking a library API function from the command-line has not been tested yet:

```
    $ clojure -X quoll.raphael/parse "@prefix : <http://data.org/>. :a :b 5 ."
    {:namespaces {"" "http://data.org"}
     :triples [[#quoll.raphael.core.Iri{:prefix "", :local "a", :iri "http://data.org/a"}
                #quoll.raphael.core.Iri{:prefix "", :local "b", :iri "http://data.org/b"}
                5]]}
```

Run the project's tests:

    $ clojure -X:build

Run the project's CI pipeline and build a JAR (this will fail until you edit the tests to pass):

    $ clojure -T:build ci

This will produce an updated `pom.xml` file with synchronized dependencies inside the `META-INF`
directory inside `target/classes` and the JAR in `target`. You can update the version (and SCM tag)
information in generated `pom.xml` by updating `build.clj`.

Install it locally (requires the `ci` task be run first):

    $ clojure -T:build install

Deploy it to Clojars -- needs `CLOJARS_USERNAME` and `CLOJARS_PASSWORD` environment
variables (requires the `ci` task be run first):

    $ clojure -T:build deploy

The library will be deployed to net.clojars.quoll/raphael on clojars.org by default.
