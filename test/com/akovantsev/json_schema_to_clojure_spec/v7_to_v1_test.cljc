(ns com.akovantsev.json-schema-to-clojure-spec.v7-to-v1-test
  (:require
   [com.akovantsev.json-schema-to-clojure-spec.v7-to-v1 :as x]
   [cheshire.core :as json]
   [clojure.walk :as walk]
   [clojure.string :as str]))

(set! *print-namespace-maps* false) ;;fixme remove

(defn spy [x] (clojure.pprint/pprint x) x)

(defn stringify [m]
  (walk/postwalk
    #(if (keyword? %) (name %) %)
    m))

(def do-printer x/do-printer)
(def convert x/convert)
(def make-spec x/make-spec)

(def test-opts
  (merge x/default-opts
    {::x/json-key-fn identity                                 ;keywordize
     ::x/root-ns     "user"
     ::x/path        ["user" "root"]}))



(defn result [m]
  {:todo      (m ::x/todo)
   :functions (m ::x/functions)
   :enums     (m ::x/enums)
   :registry  (m ::x/registry)
   :form      (m ::x/form)})


(assert (= (x/alias-resolution-batches {:d :a :b 1 :a :b :c :a}) [{:b 1} {:a :b} {:c :a :d :a}]))
(assert (= (x/alias-resolution-batches {1 0 2 1 3 1 4 3}) [{1 0} {2 1, 3 1} {4 3}]))

(assert (= (x/curr-spec-name (x/navigate-to-ref test-opts "#/definitions/UnitSpec")) :user.definitions/UnitSpec))
(x/navigate-to-ref test-opts "#/definitions/UnitSpec")

(do-printer (make-spec test-opts {}))
(do-printer (make-spec test-opts true))
(do-printer (make-spec test-opts false))

(do-printer (make-spec test-opts {"$ref" "#/definitions/UnitSpec"}))

(do-printer
  (convert test-opts
    {"anyOf"       [{"type" "string"}
                    {"$ref" "#/definitions/UnitSpec"}]
     "definitions" {"UnitSpec" {"type" "number"}}}))


(do-printer (make-spec test-opts {"not" {"type" "number" "exclusiveMaximum" 10}}))

(do-printer (make-spec test-opts {"enum" [1]}))
(do-printer (make-spec test-opts {"enum" [1 nil]}))
(do-printer (make-spec test-opts {"enum" [1 nil false]}))


(do-printer (make-spec test-opts {"type" "string" "enum" ["x" "y" "z"]}))

(do-printer (make-spec test-opts {"type" "number"}))
(do-printer (make-spec test-opts {"type" "null"}))
(do-printer (make-spec test-opts {"type" ["null"]}))
(do-printer (make-spec test-opts {"type" ["number"]}))
(do-printer (make-spec test-opts {"type" ["null" "number"]}))
(do-printer (make-spec test-opts {"type" ["null" "number" "string"]}))


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


(do-printer
  (make-spec test-opts
    {"type"     "array"
     "minItems" 1
     "maxItems" 5
     "contains" {"type" "string"}
     "items"    {"$ref" "#/definitions/Align"}}))


(do-printer
  (make-spec test-opts
    {"type" "array",
     "minItems" 1,
     "maxItems" 5,
     "contains" {"type" "number"},
     "items" {"type" "number", "exclusiveMaximum" 10}}))

(do-printer
  (make-spec test-opts
    {"type"     "array"
     "minItems" 1
     "maxItems" 5
     "contains" {"type" "number" "exclusiveMinimum" 2}
     "items"    {"type" "number" "exclusiveMaximum" 10}}))

(do-printer
  (make-spec test-opts
    {"type"     "array"
     "minItems" 1
     "maxItems" 5
     "items"    {"type" "number" "exclusiveMaximum" 10}}))


(do-printer (make-spec test-opts {"not" {"type" "object", "properties" {"foo" {"type" "number"}}}}))

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

(do-printer
  (convert test-opts
    {"type"                 "object",
     "additionalProperties" {"type" "string"},
     "minProperties"        1,
     "maxProperties"        4,
     "required"             ["numbers"],
     "properties"           {"numbers"     {"type"            "array",
                                            "items"           [{"type" "number"}],
                                            "additionalItems" {"type" "string", "enum" ["a" "b" nil]}},
                             "street_name" {"type" "string"},
                             "street_type" {"type" "string", "enum" ["Street" "Avenue" "Boulevard"]}}}))


(do-printer
  (make-spec test-opts
    {"type"          "object"
     "propertyNames" {"type"      "string"
                      "minLength" 3
                      "maxLength" 5}}))




;;fixme inc path for multispec
(do-printer (make-spec test-opts {"type" ["null" "number"] "exclusiveMaximum" 20}))
(do-printer (make-spec test-opts {"type" "integer" "multipleOf" 2/5}))
(do-printer (make-spec test-opts {"anyOf" [{"type" "integer", "multipleOf" 2/5, "minimum" 10, "exclusiveMaximum" 20}
                                           {"type" "integer", "multipleOf" 3/5}
                                           {"type" "integer"}]}))

(do-printer (x/-num-spec test-opts {"type" "integer", "multipleOf" 2, "minimum" 10, "exclusiveMaximum" 20} 'int?))


(do-printer (make-spec test-opts {"$ref" "#/definitions/LayerSpec"}))

(do-printer
  (make-spec test-opts
    {"type"            "array",
     "minItems"        3,
     "additionalItems" {"type" ["number" "null"]},
     "contains"        {"type" "string"},
     "items"           {"anyOf" [{"$ref" "#/definitions/LayerSpec"}
                                 {"$ref" "#/definitions/UnitSpec"}]}}))


(assert (= (x/split-vec 3 [0 1 2 3 4]) [3 [0 1 2 4]]))





(do-printer
  (convert test-opts {"type" "number", "minimum" 5, "maximum" 10}))

(do-printer
  (convert test-opts {"type" "array", "minItems" 5, "maxItems" 10}))



(do-printer
  (make-spec test-opts
    {"type"                 "object",
     "additionalProperties" {"type" "number", "minimum" 5, "maximum" 10},
     "properties"           {"frame"   {"description" "A frame specification as a two-element array used to control the window over which the specified method is applied. The array entries should either be a number indicating the offset from the current data object, or null to indicate unbounded rows preceding or following the current data object. For example, the value `[-5, 5]` indicates that the window should include five objects preceding and five objects following the current object.

                                       __Default value:__:  `[null, null]` indicating that the window includes all objects.",
                                        "items"       [{"type" ["null" "number"]} {"type" ["null" "number"]}],
                                        "maxItems"    2,
                                        "minItems"    2,
                                        "type"        "array"},
                             "keyvals" {"description" "Defines the key values that should be considered for imputation.
                                         An array of key values or an object defining a [number sequence](https://vega.github.io/vega-lite/docs/impute.html#sequence-def).

                                         If provided, this will be used in addition to the key values observed within the input data. If not provided, the values will be derived from all unique values of the `key` field. For `impute` in `encoding`, the key field is the x-field if the y-field is imputed, or vice versa.

                                         If there is no impute grouping, this property _must_ be specified.",
                                        "anyOf"       [{"items" {"type" "number"}, "type" "array"} {"$ref" "#/definitions/ImputeSequence"}]},
                             "method"  {"$ref"        "#/definitions/ImputeMethod",
                                        "description" "The imputation method to use for the field value of imputed data objects.
                                        One of `\"value\"`, `\"mean\"`, `\"median\"`, `\"max\"` or `\"min\"`.

                                        __Default value:__  `\"value\"`"},
                             "value"   {"description" "The field value to use when the imputation `method` is `\"value\"`."}}}))


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

(do-printer
  (convert test-opts
    {"oneOf" [{"$ref" "$/refs/a"}
              {"$ref" "$/refs/b"}
              {"$ref" "$/refs/c"}]
     "refs"  {"a" {"enum" ["a" nil 2]}  ;;fixme: enum name is too short in
              "b" {"enum" ["b" nil 5]}  ;;review: literal json objects in the enums are allowed
              "c" {"type" "object"
                   "properties" {"foo" {"type" "integer" "multipleOf" 3.5}
                                 "bar" {"type" "string" "minLength" 10}}
                   "required" ["bar"]}}}))

(do-printer (convert test-opts {"const" true}))
(do-printer (convert test-opts {"const" false}))
(do-printer (convert test-opts {"const" nil}))
(do-printer (convert test-opts {"const" "foo"}))

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
(defn keywordize
  ([x] (keywordize nil x))
  ([ns x]
   (keyword
     (some-> ns name)
     (-> x
       munge
       (str/replace "," "_COMMA_")
       (str/replace "(" "_LP_")
       (str/replace ")" "_RP_")))))

(keywordize "FieldOrDatumDef_WithCondition<MarkPropFieldDef,(Gradient|string|null)>")
(keywordize "ValueDef<(number|\"height\")>")


(def url "https://vega.github.io/schema/vega/v5.json")
(defonce schema (json/parse-string (slurp url) keywordize))

schema
(-> schema :allOf)
(-> schema keys)
(-> schema :refs keys)
(-> schema :defs keys)
(-> schema (get "definitions") keys)

(def test-opts2 (assoc test-opts ::x/schema schema ::x/json-key-fn keywordize))

(x/get-ref-schema test-opts2 "#/defs/scope")

(result (make-spec test-opts2 schema))
(result (make-spec test-opts2 {:type "object"}))

(x/schema-type test-opts2 {:type "object"})
(spit "/tmp/2.cljc"
  (apply list
    (do-printer
      (convert test-opts2 schema))))


;; todo:
;      "properties": {
;        "$schema": {
;          "type": "string",
;          "format": "uri"
;        },
;;todo:
;;        "config": {
;          "type": "object"
;        },
;
#_*e
#_(eval *1)
#_(-> :user/root (s/exercise 10) (->> (map first)))
