```
ALPHA QUALITY
``` 
## MOTIVATION

Once upon a time, I decided to give 
https://vega.github.io/vega-lite/
a try: declarative "API as json" and all.
However
https://vega.github.io/schema/vega-lite/v4.json
is `20kloc` and 
https://vega.github.io/schema/vega/v5.json
is `16kloc`, and none of it is flat and linear. 
So, exploring what can be done is problematic (let's appreciate the fact those exist at all!).


## INTENDED USE
Generate spec forms, print them as giant `do` or `ns` block, paste it to `.cljc` file, then: REPL, IDE navigation, `s/exercise`, etc.
 

## GOALS

##### Support all of the schema features
Regardless of misalignment with spec philosophy (openness). This results in a bunch of cringy predicates.
For spec1, this, combined with `s/and`'s flowing of conformed values further uglyfies output.

##### Make spec tree mirror schema
To avoid extra transformations you would have to keep in mind while eyeballing input schema and output spec side by side.

##### Extensibility
Even though output spec can be manually edited at will, I'd like to enable overriding specs and generators as an input.


## NON GOALS

##### Not a schema validation library.
If schema is invalid, you might get `unknown schema type` error, or some exception halfway through spec generation.

##### Not a schema analyzer.
If schema is unsatisfiable – generated spec will be unsatisfiable too, without any dedicated warnings.

##### Not a realtime translator
Generated output is a (not yet "applied") spec registry, not a deep inline anonymous spec (at least for anything non-trivial, like `{"type": "string"}`).
To validate against generated spec, you'd have to register generated registry "for real".
If you do this, you likely gonna have a bad time.

However, anonymous specs are just 1 tree-traversal function away (assuming you solve `s/keys` attributes collision potential).

##### Not (THAT kind of) a generated specs optimizer
Generated specs generated to mirror source schema as close as possible, so you would not have to mind optimizations when/if debugging stuff.
There might be few exceptions to this in the most trivial cases (like `{"type": ["null" "string"]}`, where result is `(s/nilable number?)` rather than `s/or`),
but those make walking conformed values irregular and different from walking the source schema, so I am still on the fence with this. 

##### Convert schemas to anything other than spec
Conversion to
https://github.com/clojure/spec-alpha2
will be done in this repo in different namespace. 

Conversions to:
 - https://github.com/metosin/malli
 - https://github.com/plumatic/schema
 - etc

might be done someday, but in separate repos.

##### String formats
While it somewhat contradicts the `support all schema features` goal, re-writing regexp and various string format schemas (`datetime`, `email`, etc.) and generators for those – is out of scope of the library.
Maybe later. 
TBA

## USAGE
```clojure
(ns foo.bar
  (:require 
   [com.akovantsev.json-schema-to-clojure-spec.v7-to-v1 :as ss]
   [cheshire.core :as json]))

(defonce schema (json/parse-string (slurp "https://vega.github.io/schema/vega-lite/v4.json")))

(ss/do-printer ;;todo custom keywordize fn for vega's bs class names
  (ss/make schema ss/default-opts))
```
In this particular example, schema vega entity names are (probably) generated from type script class signatures, hence custom `keywordyze` function.  
