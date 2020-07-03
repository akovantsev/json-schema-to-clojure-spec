(ns com.akovantsev.json-schema-to-clojure-spec.v7-to-v1
  (:require
   [com.akovantsev.json-schema-to-clojure-spec.v7-to-v1 :as ss]
   [cheshire.core :as json]
   [clojure.walk :as walk]))

(set! *print-namespace-maps* false) ;;fixme remove

(defn spy [x] (clojure.pprint/pprint x) x)

(defn stringify [m]
  (walk/postwalk
    #(if (keyword? %) (name %) %)
    m))



(def test-opts
  (merge default-opts
    {::json-key-fn identity                                 ;keywordize
     ::root-ns     "user"
     ::path        ["user" "root"]}))



(defn result [m]
  {:todo      (m ::todo)
   :functions (m ::functions)
   :enums     (m ::enums)
   :registry  (m ::registry)
   :form      (m ::form)})


(assert (= (alias-resolution-batches {:d :a :b 1 :a :b :c :a}) [{:b 1} {:a :b} {:c :a :d :a}]))
(assert (= (alias-resolution-batches {1 0 2 1 3 1 4 3}) [{1 0} {2 1, 3 1} {4 3}]))

(assert (= (curr-spec-name (navigate-to-ref test-opts "#/definitions/UnitSpec")) :user.definitions/UnitSpec))


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

(do-printer (-num-spec test-opts {"type" "integer", "multipleOf" 2, "minimum" 10, "exclusiveMaximum" 20} 'int?))


(do-printer (make-spec test-opts {"$ref" "#/definitions/LayerSpec"}))

(do-printer
  (make-spec test-opts
    {"type"            "array",
     "minItems"        3,
     "additionalItems" {"type" ["number" "null"]},
     "contains"        {"type" "string"},
     "items"           {"anyOf" [{"$ref" "#/definitions/LayerSpec"}
                                 {"$ref" "#/definitions/UnitSpec"}]}}))


(assert (= (split-vec 3 [0 1 2 3 4]) [3 [0 1 2 4]]))



;(->> vega-json :definitions :Transform make)
;(->> vega-json :definitions :Color make)
;(->> vega-json :definitions :ColorName make)
;(->> vega-json :definitions :UtcMultiTimeUnit make) ; eval s/exercise (map first))



(do-printer
  (convert test-opts {"type" "number", "minimum" 5, "maximum" 10}))

(do-printer
  (convert test-opts {"type" "array", "minItems" 5, "maxItems" 10}))


;#_
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
              {"type" "number", "exclusiveMaximum" 2}
              {"type" "number", "exclusiveMaximum" 3}]}))



#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
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

(def test-opts2 (assoc test-opts ::schema schema ::json-key-fn keywordize))

(get-ref-schema test-opts2 "#/defs/scope")

(result (make-spec test-opts2 schema))
(result (make-spec test-opts2 {:type "object"}))

(schema-type test-opts2 {:type "object"})
(spit "/tmp/2.cljc"
  (apply list
    (do-printer
      (convert test-opts2 schema))))

(do-printer
  (convert test-opts2
    {:oneOf [{:$ref "$/refs/a"}
             {:$ref "$/refs/b"}
             {:$ref "$/refs/c"}]
     :refs {:a {:enum ["a" nil]}
            :b {:enum ["b" nil]}
            :c {:enum ["c" nil]}}}))

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

#_*e
#_(eval *1)
#_(-> :user/root (s/exercise 10) (->> (map first)))
