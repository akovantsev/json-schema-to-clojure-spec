(ns com.akovantsev.json-schema-to-clojure-spec.v7-to-v1
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.spec.alpha :as s]))

;; terminology:
;; `spec` is spec name, keyword (or symbol for inline specs, like `string?` and sets).
;; `form` is spec s-expression, edn data structure.
;; e.g. (s/def spec form)
;; `schema` json-schema edn map, e.g. {"type" "string"}
;; `path` represents how deep we are in the model tree, and is used to make `specs`.
;; `ref` is a shema id – json-schema's uri, reference to schema described elsewhere in toplevel schema.
;; `registry` - `spec - form` pairs
;; `todo` - `ref - spec` pairs


;;fixme: limit s/cat arity if items limit is given
;;fixme: generators for num predicates;; no, add support of supplying custom generators instead.
;;fixme: 2 levels of indirection dependencies still fail if order is not right
;;fixme: include "description" as comments on specs
;;fixme: Invalid token: :user.root/0
;;fixme: move nil? from s/or to s/nillable
;;fixme: move nil and false from enums; or, instead: def set as a var, and wrap it in "s/nillable s/or false?" spec
;;fixme: make munge/demunge fns configurable
;;fixme: (s/keys :req-un []) -> (s/keys)
;;fixme: s/keys -> (s/or :qualified (s/keys :req [,,,]) :unqualified (s/keys :req-un [,,,]))
;;fixme: req/opt in s/keys
;;fixme: cannot use s/nonconforming with or, because it propagates all the way down to leafs, but need to know when to "unform" inside s/and, e.g. for s/cat count limits.
;;fixme: push aliases to the top of file
;;fixme: Unable to resolve spec: :user1.Axis/labelAngle
;;fixme: this is "too deep recursion" error, just try again: Unable to construct gen at: [:i4 :user1.FilterTransform/filter :i2 :user1.LogicalOr_LT_Predicate_GT_/or :i2 :user1.LogicalOr_LT_Predicate_GT_/or :i2 :user1.LogicalOr_LT_Predicate_GT_/or :i0 :user1.LogicalNot_LT_Predicate_GT_/not :i1 :user1.LogicalAnd_LT_Predicate_GT_/and :i3 :i8 :user1.SelectionPredicate/selection :i0 :user1.SelectionNot/not :i2 :user1.SelectionOr/or :i1 :user1.SelectionAnd/and :i2 :user1.SelectionOr/or] for: :user1/SelectionComposition



(defn get-ref-schema [state ref-path]
  (if (nil? ref-path)
    (throw (ex-info "ref path is nil" {(keyword "state") state}))
    (if-let [schema (->> (str/split ref-path #"/")
                       (rest)
                       (map (::json-key-fn state))
                       (get-in (::schema state)))]
      schema
      (throw
        (ex-info "top level schema is missing reference declaration:"
          {(keyword "ref")   ref-path
           (keyword "state") state})))))


(defn call-form [fn-form & args]
  ;;(call-form '(defn foo [a b]) 'x 'y) ;=> (foo x y)
  (apply list (second fn-form) args))




(defn num-kw [n] (->> n (str "i") keyword))

(defn schema-type [state x]
  (case x
    true  ::any
    false ::none
    ;else:
    (let [kf        (::json-key-fn state)
          noop-keys ["description" "$schema"]
          empty-map (->> noop-keys
                      (map kf)
                      (reduce dissoc x)
                      (= {}))
          err       (ex-info "unknown schema type:" {(keyword "schema") x})]
      (assert (map? x) (str "not a map schema: " x))

      (cond
        empty-map                  ::any
        (contains? x (kf "not"))   ::not
        (contains? x (kf "$ref"))  ::ref
        (contains? x (kf "anyOf")) ::anyOf
        (contains? x (kf "oneOf")) ::oneOf
        (contains? x (kf "allOf")) ::allOf
        (contains? x (kf "const")) ::const ;;todo
        (contains? x (kf "enum"))  ::enum
        (contains? x (kf "properties")) ::object
        (contains? x (kf "type"))
        (let [t (-> "type" kf x)]
          (if (vector? t)
            ::multitype
            (case t
              "array"   (let [items (-> "items" kf x)]
                          (cond
                            (-> items map?)    ::list
                            (-> items vector?) ::tuple
                            (-> items nil?)    ::array-of-any
                            :else              (throw err)))
              "integer" ::integer
              "number"  ::number
              "boolean" ::boolean
              "null"    ::null
              "string"  ::string
              "object"  (if (contains? x (kf "propertyNames"))
                          ::map-of
                          ::object))))
        :else
        (throw err)))))

(defn enum-name [state]
  (-> state ::path rest
    (->>
      (map name)
      (str/join "-")
      (symbol))))

(def make-spec nil)
(defmulti make-spec
  (fn [state schema]
    ((::dispatch-fn state) state schema)))

(defn alias-resolution-batches [kvs]
  (let [ks #(-> % keys set)]
    (loop [done         []
           not-resolved (ks kvs)
           todo         kvs
           aliases      {}
           originals    {}]
      (let [[k v] (first todo)
            todo  (dissoc todo k)]
        (if (nil? k)
          (if (empty? aliases)
            (conj done originals)
            (recur (conj done originals) (ks aliases) aliases {} {}))
          (if (contains? not-resolved v)
            (recur done not-resolved todo (assoc aliases k v) originals)
            (recur done not-resolved todo aliases (assoc originals k v))))))))




(defn spec-def [spec form]
  (list 's/def spec form))

(defn core-def [[k v]]
  (list 'def k v))


(defn navigate-to-ref [state ref-path]
  ;; set ::path to ref-path
  (let [kf  (::json-key-fn state)]
    (-> ref-path
      (str/split #"/")
      (rest)
      (->>
        (map kf)
        (into [(::root-ns state)])
        (assoc state ::path)))))

(defn curr-spec-name [state]                               ;;fixme api
  (keyword;(::json-key-fn state) ;;fixme: this fn has to be configurable
   (->> state ::path pop (map name) (str/join "."))
   (->> state ::path peek name)))

(defn ref-spec-name [state ref-path]
  (-> state
    (navigate-to-ref ref-path)
    (curr-spec-name)))




(s/def ::dispatch-fn ifn?)
(s/def ::enum-name-fn ifn?)
(s/def ::json-key-fn ifn?)
(s/def ::ref-spec-name-fn ifn?)
(s/def ::root-spec qualified-keyword?)
(s/def ::qualified? (s/nilable boolean?))
(s/def ::schema (s/or :boolean boolean? :map map?))
(s/def ::options-map
  (s/keys
    :req [::dispatch-fn
          ::enum-name-fn
          ::json-key-fn
          ::ref-spec-name-fn
          ::qualified?
          ::root-spec
          ::schema]))

(s/def ::path-segment
  (s/with-gen
    #(try (-> % name empty? not) (catch Exception e false))
    #(s/gen string?)))

(s/def ::path (s/coll-of ::path-segment :min-count 2 :into []))



(def default-opts
  {::dispatch-fn      schema-type
   ::enum-name-fn     enum-name
   ::json-key-fn      keyword
   ::ref-spec-name-fn ref-spec-name
   ::qualified?       false
   ::root-spec        :user/root
   ::functions        #{}})


(defn register [registry spec form]
  (if (= spec form)
    registry
    (let [[s f] (find registry spec)]
      (cond
        (nil? s)   (assoc registry spec form)
        (= f form) registry
        :else      (throw
                     (ex-info (str "Refusing to overwrite: " spec f)
                       {:name spec :old-form f :new-form form}))))))

(defn merge-registry [reg1 reg2]
  (reduce-kv register reg1 reg2))

(defn -merge-state [state1 state2]
  (-> state1
    (update ::registry merge-registry (::registry state2))
    (update ::enums merge-registry (::enums state2))
    (update ::todo merge (::todo state2))
    (update ::functions into (::functions state2))))

(defn -merge-states [state states]
  (reduce -merge-state state states))

(defn do-printer [state]
  (let [{::keys [root-spec enums functions registry form todo]} state]
    (let [batches (alias-resolution-batches registry)
          pathify (fn [kw] (str (namespace kw) "." (name kw)))
          sdefs   (fn [kvs]
                    (->> kvs
                      (sort-by #(-> % key pathify))
                      (map (partial apply spec-def))))]
      (remove nil?
        (concat
          (list 'do)
          (when (seq todo) [todo])
          ;(list 'ns (symbol root-ns) '(:require [clojure.spec.alpha :as s]))
          (map core-def enums) ;;fixme results in nil, so it is [nil] sometimes.
          (sort-by str functions)
          (mapcat sdefs batches)
          (when-not (or (= root-spec form) (contains? registry root-spec))
            (sdefs {root-spec form})))))))



(defn -set-form ;;review: split into 3 functions with more descriptive names
  ([state] (-set-form state nil))
  ([state form] (-set-form state form symbol? keyword?))
  ([state form & no-op-preds]
   (let [form   (or form (::form state))
         no-op? (apply some-fn no-op-preds)]
     (if (no-op? form)
       (assoc state ::form form)
       (let [spec (curr-spec-name state)]
         (-> state
           (assoc ::form spec)
           (update ::registry register spec form)))))))



(defn convert ;;fixme
  ([schema] (convert schema nil))
  ([opts schema]
   (let [{::keys [root-spec]} opts
         root-ns   (namespace root-spec)
         state     (merge
                     default-opts
                     opts
                     {::schema  schema
                      ::root-ns root-ns
                      ::path    [root-ns (name root-spec)]})
         errors    (s/explain-data ::options-map state)]
     (assert (nil? errors) errors)
     (loop [state (make-spec state schema)]
       (let [done  (->> state ::registry keys set)
             done? #(-> % val done)
             todo  (->> state ::todo (remove done?) (into {}))
             [ref-path spec] (first todo)]
         ;(println ref-path)
         (if-not ref-path
           state
           (-> state
             (assoc ::path [(namespace spec) (name spec)])         ;;review path
             (assoc ::todo (dissoc todo ref-path))
             (make-spec (get-ref-schema state ref-path))
             (as-> $
               (-set-form $ (::form $) (constantly false)))
             (recur))))))))

(defmethod make-spec ::array-of-any [state schema]
  (let [kf (::json-key-fn state)]
    (make-spec state (assoc schema (kf "items") {}))))

(defmethod make-spec ::any [state schema]
  (assoc state ::form 'any?))


(defmethod make-spec ::none [state schema]
  (assoc state ::form '(s/with-gen #{} (fn [] (s/gen #{::s/invalid})))))


(defmethod make-spec ::null [state m]
  (assoc state ::form 'nil?))


(defmethod make-spec ::boolean [state m]
  (assoc state ::form 'boolean?))


(defmethod make-spec ::ref [state schema]
  (let [kf   (::json-key-fn state)
        sf   (::ref-spec-name-fn state)
        path (-> "$ref" kf schema)
        spec (sf state path)]
    (-> state
      (assoc ::form spec)
      (update ::todo assoc path spec))))


(def conformed-valid-fn-form
  '(defn conformed-valid? [unform-spec next-spec]
     (fn [conformed-val]
       (->> conformed-val
         (s/unform unform-spec)
         (s/valid? next-spec)))))

(defmethod make-spec ::allOf [state m]
  ;(clojure.pprint/pprint m)
  (let [kf      (::json-key-fn state)
        schemas (-> "allOf" kf m)
        states  (->> schemas
                  (map-indexed
                    (fn [idx item]
                      (-> state
                        (update ::path conj (str "i" idx))
                        (make-spec item)
                        ;(assoc ::branch (num-kw idx))
                        (-set-form)))))


        form    (case (count states)
                  0 'any?
                  1 (-> states first ::form)
                  ;else:
                  (let [[head & tail] (map ::form states)
                        unform (fn [spec]
                                 (call-form conformed-valid-fn-form head spec))]
                    (concat ['s/and head] (map unform tail))))]
    (-> state
      (-merge-states states)
      (update ::functions conj conformed-valid-fn-form)
      (-set-form form))))


(defmethod make-spec ::anyOf [state m]
  (let [kf           (::json-key-fn state)
        all-schemas  (-> "anyOf" kf m distinct) ;;set changes original anyOf schemas order
        null-schema  {(kf "type") "null"} ;;review: nilable vs null s/or branch
        some-schemas (remove #{null-schema} all-schemas)
        nilable?     (not= (count all-schemas) (count some-schemas))
        states       (->> some-schemas
                       (map-indexed
                         (fn [idx item]
                           (-> state
                             (update ::path conj (str "i" idx))
                             (make-spec item)
                             (assoc ::branch (num-kw idx))
                             (-set-form)))))

        form         (cond
                       (-> states count zero?)  nil
                       (-> states count (= 1))  (-> states first ::form)
                       :else                    (cons 's/or
                                                  (mapcat (juxt ::branch ::form) states)))
        form         (cond
                       (and form nilable?) (update state ::form #(list 's/nilable %))
                       nilable?            (assoc state ::form 'nil?)
                       form                form
                       :else               'any?)] ;; not sure if valid empty :anyOf json-schema
    (-> state
      (-merge-states states)
      (-set-form form))))



(def min-fn-form
  '(defn min-count [coll-spec nmin]
     (fn min-count [conformed]
       (<= nmin (count (s/unform coll-spec conformed))))))

(def max-fn-form
  '(defn max-count [coll-spec nmax]
     (fn max-count [conformed]
       (>= nmax (count (s/unform coll-spec conformed))))))

(def distinct-fn-form
  '(defn all-distinct [coll-spec]
     (fn [conformed]
       (->> conformed (s/unform coll-spec) (apply distinct?)))))

(defn -make-and [forms]
  (let [form (remove nil? forms)]
    (if (-> form count (= 1))
      (first form)
      (cons 's/and form))))


;; https://json-schema.org/understanding-json-schema/reference/string.html#length
(defmethod make-spec ::string [state m]
  (let [kf        (::json-key-fn state)
        minLength (-> "minLength" kf m)
        maxLength (-> "maxLength" kf m)
        spec      'string?
        functions (->> [(when minLength min-fn-form)
                        (when maxLength max-fn-form)]
                    (remove nil?)
                    (set))]
    (-> state
      (assoc ::functions functions)
      (-set-form
        (-make-and
          [spec
           (when minLength (call-form min-fn-form spec minLength))
           (when maxLength (call-form max-fn-form spec maxLength))])))))



(defn -comparator-fn-name [comp-sym limit]
  (-> (str comp-sym limit "?")
    (str/replace "/" "over") ;; for ratios: 1/3
    (str/replace "." "-")    ;; for decimals: 1.3
    (symbol)))

(defn -num-spec [state m sym]
  (let [kf      (::json-key-fn state)

        [minimum exclusiveMinimum exclusiveMaximum maximum multipleOf]
        (map #(-> % kf m)
          ["minimum" "exclusiveMinimum" "exclusiveMaximum" "maximum" "multipleOf"])

        fn-form (fn [comp-sym limit]
                  (when limit
                    (let [fname (-comparator-fn-name comp-sym limit)]
                      (list 'defn fname '[x]
                        (list comp-sym 'x limit)))))
        forms  (remove nil?
                 [(fn-form '>= minimum)
                  (fn-form '> exclusiveMinimum)
                  (fn-form '< exclusiveMaximum)
                  (fn-form '<= maximum)
                  (when multipleOf
                    (let [fname (-comparator-fn-name "mod" multipleOf)]
                      (list 'defn fname '[x]
                        (list 'zero?
                          (list 'mod 'x multipleOf)))))])
        syms    (map second forms)
        form    (-make-and (cons sym syms))]
    (-> state
      (assoc ::functions (set forms))
      (-set-form form))))


(defmethod make-spec ::integer [state m] (-num-spec state m 'int?))
(defmethod make-spec ::number  [state m] (-num-spec state m 'number?))


(defmethod make-spec ::not [state schema]
  (let [kf     (::json-key-fn state)
        state2 (make-spec
                 (update state ::path conj "not")
                 (-> "not" kf schema))
        form   (list 's/with-gen
                 (list 'fn '[x] (list 'not (list 's/valid? (::form state2) 'x)))
                 (list 'fn '[ ] (list 's/gen 'any?)))]
    (-> state
      (-merge-state state2)
      (-set-form form))))



(defmethod make-spec ::enum [state m]
  ;;fixme: need to s/and with the :type
  (let [kf       (::json-key-fn state)
        enum     (-> "enum" kf m set)
        enumname ((::enum-name-fn state) state)
        pairs    (concat
                   [:enum enumname]
                   (when (contains? enum false) [:false 'false?])
                   (when (contains? enum nil) [:nil 'nil?]))
        form     (if (-> pairs count (= 2))
                   (second pairs)
                   (list 's/nonconforming (cons 's/or pairs)))]
    (-> state
      (update ::enums register enumname enum)
      (-set-form form))))


(def includes-fn-form
  '(defn includes? [coll-spec item-spec]
     (fn [coll]
       (some
         (fn [x] (s/valid? item-spec x))
         (s/unform coll-spec coll)))))

(defn -and-contains [state contains]
  (if-not contains
    state
    (let [items-state (-> state
                        (update ::path conj "items")
                        ;;already has ::form
                        (-set-form))
          cont-state  (-> state
                        (update ::path conj "contains")
                        (make-spec contains))
          form        (list 's/and (::form items-state)
                        (call-form includes-fn-form (::form items-state) (::form cont-state)))]
      (-> state
        (-merge-state items-state)
        (-merge-state cont-state)
        (update ::functions conj includes-fn-form)
        (assoc ::form form)))))


;; https://json-schema.org/understanding-json-schema/reference/array.html#list-validation
(defmethod make-spec ::list [state schema]
  (let [kf          (::json-key-fn state)

        [items minItems maxItems contains uniqueItems]
        (map #(-> % kf schema)
          ["items" "minItems" "maxItems" "contains" "uniqueItems"])

        items-state (-> state
                      (update ::path conj "item")
                      (make-spec items))
        form        (concat
                      ['s/coll-of (::form items-state)]
                      [:into []] ;;for repl read/write symmetry
                      (when uniqueItems [:distinct true])
                      (when minItems [:min-count minItems])
                      (when maxItems [:max-count maxItems]))]
    (-> state
      (-merge-state items-state)
      (assoc ::form form)
      (-and-contains contains)
      (-set-form))))


;;fixme: references can be stored in the different branches of schema, so names can clash -> need to prefix them with ref path.



;; https://json-schema.org/understanding-json-schema/reference/array.html#tuple-validation
(defmethod make-spec ::tuple [state schema]
  (let [kf          (::json-key-fn state)
        [items additionalItems minItems maxItems contains uniqueItems]
        (map #(-> % kf schema)
          ["items" "additionalItems" "minItems" "maxItems" "contains" "uniqueItems"])

        item-states (->> items
                      (map-indexed
                        (fn [idx item]
                          (let [state (update state ::path conj "items" (str "i" idx))]
                            (make-spec state item))))
                      (vec))

        extra-state (case additionalItems
                      false nil
                      nil {::form 'any?}
                      (make-spec (update state ::path conj "items" "additionalItem") additionalItems))

        extra       (::form extra-state)
        idxs        (map num-kw (range))
        forms       (map ::form item-states)
        pairs       (interleave idxs forms)
        form        (cons 's/cat
                      (concat pairs
                        (when extra [(keyword "&") (list 's/* extra)])))

        and?        (or minItems maxItems uniqueItems)
        spec        (when and? (curr-spec-name (update state ::path conj "items")))
        base        (if-not and?
                      form
                      (-make-and
                        [spec
                         (when minItems (call-form min-fn-form spec minItems))
                         (when maxItems (call-form  max-fn-form spec maxItems))
                         (when uniqueItems (call-form distinct-fn-form spec))]))]
    (-> state
      (-merge-states item-states)
      (-merge-state extra-state)
      (cond->
        and?        (update ::registry register spec form)
        minItems    (update ::functions conj min-fn-form)
        maxItems    (update ::functions conj max-fn-form)
        uniqueItems (update ::functions conj distinct-fn-form))
      (assoc ::form base)
      (-and-contains contains)
      (-set-form))))


;; describe nilable decision.

;; i0 instead 0 is because :0 is a valid keyword, but :foo/0 is not. I want to preserve symmetry between branch and spec names.

;;todo: add `:into []` for array specs to avoid literal lists in output

(defmethod make-spec ::multitype [state schema]
  ;; can wrap in nilable here.
  (let [kf       (::json-key-fn state)
        tk       (kf "type")
        types    (-> tk schema distinct)
        sf       (fn sf [t]
                   (-> state
                     (update ::path conj t)
                     (make-spec (assoc schema tk t))
                     (assoc ::branch (keyword t))))
        states   (map sf types)
        form     (case (count states)
                   0 'any?
                   1 (-> states first ::form)
                   ;else:
                   (cons 's/or
                     (mapcat (juxt ::branch ::form) states)))]
    (-> state
      (-merge-states states)
      (-set-form form))))

;;review: need token with a name different from any of map keys:
(defn make-unique-token [blacklist token]
  (loop [token token]
    (if (contains? blacklist token)
      (recur (str (name token) "-"))
      token)))


(def map-difference-fn-form
  '(defn map-difference [conformed-map s-keys-spec]
     (let [{:keys [opt-un req-un req opt]} (->> s-keys-spec s/form rest (apply hash-map))
           simple-keys (->> (concat opt-un req-un) (map name) (map keyword))]
       (as-> conformed-map $
         (s/unform s-keys-spec $)
         (apply dissoc $ (concat simple-keys req opt))))))



;;todo https://json-schema.org/understanding-json-schema/reference/object.html#property-names
(defmethod make-spec ::map-of [state schema]
  (let [kf          (::json-key-fn state)
        k           "propertyNames"
        keys-schema  (merge
                       {(kf "type") "string"}
                       (-> k kf schema))
        keys-state  (-> state
                      (update ::path conj k)
                      (make-spec keys-schema))
        vals-spec   (-> state
                      (update ::path conj "propertyValues")
                      (curr-spec-name))
        vals-form   'any?
        form        (list 's/map-of
                      (::form keys-state)
                      vals-spec)]
    (-> state
      (update ::registry register vals-spec vals-form)
      (-merge-state keys-state)
      (-set-form form))))



;; https://json-schema.org/understanding-json-schema/reference/object.html
;;done https://json-schema.org/understanding-json-schema/reference/object.html#properties
;;todo additionalProperties
;;todo https://json-schema.org/understanding-json-schema/reference/object.html#required-properties
;;done https://json-schema.org/understanding-json-schema/reference/object.html#size
;;todo https://json-schema.org/understanding-json-schema/reference/object.html#dependencies
;;todo https://json-schema.org/understanding-json-schema/reference/object.html#property-dependencies
;;todo https://json-schema.org/understanding-json-schema/reference/object.html#schema-dependencies
;;todo https://json-schema.org/understanding-json-schema/reference/object.html#pattern-properties
(defmethod make-spec ::object [state schema]
  (let [kf          (::json-key-fn state)
        qualified?  (::qualified? state)
        [minProperties maxProperties properties additionalProperties required]
        (map #(-> % kf schema)
          ["minProperties" "maxProperties" "properties" "additionalProperties" "required"])

        prop-states (->> properties
                      (map (fn mf [[k v]]
                             (-> state
                               (update ::path conj k)
                               (make-spec v)
                               (-set-form nil (constantly false))))))
        prop-specs  (->> prop-states (map ::form) (set))
        req-specs   (->> required
                      (map #(curr-spec-name (update state ::path conj (kf %))))
                      (set))
        opt-specs   (set/difference prop-specs req-specs)
        base-form   (when (seq prop-specs)
                      (concat
                        ['s/keys]
                        (when (seq req-specs)
                          [(if qualified? :req :req-un)
                           (->> req-specs (sort-by str) vec)])
                        (when (seq opt-specs)
                          [(if qualified? :opt :opt-un)
                           (->> opt-specs (sort-by str) vec)])))

        base-spec   (curr-spec-name (update state ::path conj "base-props"))
        extra-state (case additionalProperties
                      nil nil
                      true nil
                      false {::form (list 'fn 'no-extra-keys ['conformed-map]
                                      (list 'empty?
                                        (call-form map-difference-fn-form 'conformed-map base-spec)))}
                      ;;else:
                      (let [val-state (-> state
                                        (update ::path conj "extra-vals")
                                        (make-spec additionalProperties))
                            kv-spec   (curr-spec-name (update state ::path conj "extra-props"))
                            kv-form   (list 's/map-of 'keyword? (::form val-state))
                            ;; todo: add extra props gen
                            form      (list 'fn 'extra-vals ['conformed-map]
                                        (list 's/valid? kv-spec
                                          (call-form map-difference-fn-form 'conformed-map base-spec)))]
                        (-> val-state
                          (update ::registry register kv-spec kv-form)
                          (assoc ::form form))))

        form        (-make-and
                      [base-spec
                       (when minProperties (call-form min-fn-form base-spec minProperties))
                       (when maxProperties (call-form max-fn-form base-spec maxProperties))
                       (::form extra-state)])

        state       (-> state
                      (-merge-states prop-states)
                      (-merge-state extra-state)
                      (update ::functions into
                        (remove nil?
                          [(when extra-state map-difference-fn-form)
                           (when minProperties min-fn-form)
                           (when maxProperties max-fn-form)])))]
    (if (or minProperties maxProperties extra-state)
      (-> state
        (update ::registry register base-spec base-form)
        (-set-form form))
      (-> state
        (-set-form base-form)))))

      ;(-set-form form))))








(def all-invalid-fn-form
  '(defn all-invalid? [unform-spec specs]
     (fn [conformed-x]
       (let [x (s/unform unform-spec conformed-x)]
         (not-any?
           (fn [spec] (s/valid? spec x))
           specs)))))


(defn split-vec [idx v]
  [(nth v idx) (into (subvec v 0 idx) (subvec v (inc idx)))])



(defmethod make-spec ::oneOf [state schema]
  (let [kf     (::json-key-fn state)
        states (->> "oneOf" kf schema
                 (map-indexed
                   (fn [idx schema]
                     (let [branch (num-kw idx)]
                       (-> state
                         (update ::path conj branch)
                         (make-spec schema)
                         (assoc ::branch branch))))))
        forms  (mapv ::form states)
        forms  (->> forms count range
                 (mapv (fn [idx]
                         (let [[yes nos] (split-vec idx forms)]
                           (list 's/and yes (call-form all-invalid-fn-form yes nos))))))
        form   (cons 's/or
                 (interleave (map ::branch states) forms))]
    (-> state
      (-merge-states states)
      (update ::functions conj all-invalid-fn-form)
      (-set-form form))))


;; todo: https://json-schema.org/understanding-json-schema/basics.html#declaring-a-unique-identifier
