```
UNDER CONSTRACTION, ALPHA QUALITY, BUGS, CHANGES
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

## SETTINGS
TBA. For now, see tests and source.

## EXAMPLES
Here's a few, but see more in REPL, by evaling things in test namespace.
 * printing in these examples is slightly edited for readability, e.g.:
```clojure
(s/def :user/root ,,,
;; instead of:
(s/def 
:user/root ,,,
``` 

```clojure
(do-printer
  (convert test-opts
    {"type"            "array"
     "minItems"        5
     "maxItems"        7
     "items"           [{"enum" ["x" "y" "z" nil]}
                        {"type"             "number"
                         "exclusiveMaximum" 10}]
     "additionalItems" {"type" "string"
                        "enum" ["a" "b" nil]}}))
;;=>
(do
 (def root-items-i0 #{nil "z" "x" "y"})
 (def root-items-additionalItem #{nil "a" "b"})
 (defn <10? [x] (< x 10))
 (defn max-count [coll-spec nmax] (fn max-count [conformed] (>= nmax (count (s/unform coll-spec conformed)))))
 (defn min-count [coll-spec nmin] (fn min-count [conformed] (<= nmin (count (s/unform coll-spec conformed)))))
 (s/def :user/root (s/and :user.root/items (min-count :user.root/items 5) (max-count :user.root/items 7)))
 (s/def :user.root/items
  (s/cat :i0 :user.root.items/i0 :i1 :user.root.items/i1 :& (s/* :user.root.items/additionalItem)))
 (s/def :user.root.items/additionalItem (s/nonconforming (s/or :enum root-items-additionalItem :nil nil?)))
 (s/def :user.root.items/i0 (s/nonconforming (s/or :enum root-items-i0 :nil nil?)))
 (s/def :user.root.items/i1 (s/and number? <10?)))
```

```clojure
(do
 (defn <20? [x] (< x 20))
 (defn >=10? [x] (>= x 10))
 (defn mod2over5? [x] (zero? (mod x 2/5)))
 (defn mod3over5? [x] (zero? (mod x 3/5)))
 (s/def :user/root (s/or :i0 :user.root/i0 :i1 :user.root/i1 :i2 int?))
 (s/def :user.root/i0 (s/and int? >=10? <20? mod2over5?))
 (s/def :user.root/i1 (s/and int? mod3over5?)))
(do-printer
  (convert test-opts
    {"type"                 "object",
     "additionalProperties" {"type" "string"},
     "minProperties"        1,
     "maxProperties"        4,
     "properties"           {"numbers"     {"type"            "array",
                                            "items"           [{"type"       "object",
                                                                "properties" {"foo" {"type" "number"},
                                                                              "bar" {"type" "string", "enum" ["x" "y" "z"]}}}
                                                               {"type" "number"}],
                                            "additionalItems" {"type" "string", "enum" ["a" "b"]}},
                             "street_name" {"type" "string"},
                             "street_type" {"type" "string", "enum" ["Street" "Avenue" "Boulevard"]}}}))
;;=>
(do
 (def root-numbers-items-i0-bar #{"z" "x" "y"})
 (def root-numbers-items-additionalItem #{"a" "b"})
 (def root-street_type #{"Street" "Boulevard" "Avenue"})
 (defn map-difference [conformed-map s-keys-spec]
  (let [{:keys [opt-un req-un req opt]} (->> s-keys-spec s/form rest (apply hash-map))
        simple-keys (->> (concat opt-un req-un) (map name) (map keyword))]
   (as-> conformed-map $ (s/unform s-keys-spec $) (apply dissoc $ (concat simple-keys req opt)))))
 (defn max-count [coll-spec nmax] (fn max-count [conformed] (>= nmax (count (s/unform coll-spec conformed)))))
 (defn min-count [coll-spec nmin] (fn min-count [conformed] (<= nmin (count (s/unform coll-spec conformed)))))
 (s/def :user/root
  (s/and
   :user.root/base-props
   (min-count :user.root/base-props 1)
   (max-count :user.root/base-props 4)
   (fn extra-vals [conformed-map]
    (s/valid? :user.root/extra-props (map-difference conformed-map :user.root/base-props)))))
 (s/def :user.root/base-props (s/keys :opt-un [:user.root/numbers :user.root/street_name :user.root/street_type]))
 (s/def :user.root/extra-props (s/map-of keyword? string?))
 (s/def :user.root/numbers
  (s/cat :i0 :user.root.numbers.items/i0 :i1 number? :& (s/* root-numbers-items-additionalItem)))
 (s/def :user.root.numbers.items/i0 (s/keys :opt-un [:user.root.numbers.items.i0/bar :user.root.numbers.items.i0/foo]))
 (s/def :user.root.numbers.items.i0/bar root-numbers-items-i0-bar)
 (s/def :user.root.numbers.items.i0/foo number?)
 (s/def :user.root/street_name string?)
 (s/def :user.root/street_type root-street_type))
```


```clojure
(require '[clojure.spec.alpha :as s])

(do-printer
  (convert test-opts
    {"oneOf" [{"$ref" "$/refs/a"}
              {"$ref" "$/refs/b"}
              {"$ref" "$/refs/c"}]
     "refs"  {"a" {"enum" ["a" nil 2]}
              "b" {"enum" ["b" nil 5]}
              "c" {"type" "object"
                   "properties" {"foo" {"type" "integer" "multipleOf" 3.5}
                                 "bar" {"type" "string" "minLength" 10}}
                   "required" ["bar"]}}}))
;;=>
(do
 (def a #{nil "a" 2})
 (def b #{nil "b" 5})
 (defn all-invalid? [unform-spec specs]
  (fn [conformed-x]
   (let [x (s/unform unform-spec conformed-x)]
    (not-any? (fn [spec] (s/valid? spec x)) specs))))
 (defn min-len [nmin] (fn min-len [x] (<= nmin (count x))))
 (defn mod3-5? [x] (zero? (mod x 3.5)))
 (s/def :user.refs/a (s/nonconforming (s/or :enum a :nil nil?)))
 (s/def :user.refs/b (s/nonconforming (s/or :enum b :nil nil?)))
 (s/def :user.refs/c (s/keys :req-un [:user.refs.c/bar] :opt-un [:user.refs.c/foo]))
 (s/def :user.refs.c/bar (s/and string? (min-len 10)))
 (s/def :user.refs.c/foo (s/and int? mod3-5?))
 (s/def :user/root
  (s/or
   :i0 (s/and :user.refs/a (all-invalid? :user.refs/a [:user.refs/b :user.refs/c]))
   :i1 (s/and :user.refs/b (all-invalid? :user.refs/b [:user.refs/a :user.refs/c]))
   :i2 (s/and :user.refs/c (all-invalid? :user.refs/c [:user.refs/a :user.refs/b])))))

(eval *1)
;;=> :user/root

(map first (s/exercise :user/root))
;;=>
("b"
 "a"
 "a"
 {:bar "D9Z3xXz3hw5X4H"}
 2
 {:foo 0, :bar "HZXuPggHb1"}
 "b"
 {:foo 14, :bar "iMP6EP3u9m"}
 5
 {:foo -14, :bar "luWNw7nIn6o"})
```

```clojure
(do-printer
  (make-spec test-opts
    {"anyOf" [{"type" "number", "exclusiveMaximum" ##Inf}
              {"type" "number", "minimum" 2.4}
              {"const" "foo"}
              {"const" {"foo" "bar"}}
              {"const" nil}
              {"enum" [nil false "baz"]}
              {"const" false}
              {"type" "number", "exclusiveMaximum" 3}]}))
;;=>
(do
 (def root-i2 #{"foo"})
 (def root-i3 #{{"foo" "bar"}})
 (def root-i4 #{nil})
 (def root-i5 #{nil "baz" false})
 (def root-i6 #{false})
 (defn <3? [x] (< x 3))
 (defn <Infinity? [x] (< x ##Inf))
 (defn >=2-4? [x] (>= x 2.4))
 (s/def :user/root
  (s/or
   :i0 :user.root/i0
   :i1 :user.root/i1
   :i2 root-i2
   :i3 root-i3
   :i4 :user.root/i4
   :i5 :user.root/i5
   :i6 :user.root/i6
   :i7 :user.root/i7))
 (s/def :user.root/i0 (s/and number? <Infinity?))
 (s/def :user.root/i1 (s/and number? >=2-4??))
 (s/def :user.root/i4 (s/nonconforming (s/or :enum root-i4 :nil nil?)))
 (s/def :user.root/i5 (s/nonconforming (s/or :enum root-i5 :false false? :nil nil?)))
 (s/def :user.root/i6 (s/nonconforming (s/or :enum root-i6 :false false?)))
 (s/def :user.root/i7 (s/and number? <3?)))
```