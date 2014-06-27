(ns kibit.rules.util
  (:require [clojure.core.logic :as logic]
            [clojure.walk :as walk]))

;; wrap vectors and maps in an s-expression of form (:kibit.rules/vector ...) and
;; (:kibit.rules/map ...)
(def wrap-struct-walker 
  (partial walk/prewalk
           #(cond
             (vector? %)
             (concat `(:kibit.rules/vector) %)
             
             (map? %)
             (concat `(:kibit.rules/map) (mapcat identity %))
             
             :else %)))

(defn kibit-vector? [exp]
  (and (sequential? exp) (= :kibit.rules/vector (first exp))))

(defn kibit-map? [exp]
  (and (sequential? exp) (= :kibit.rules/map (first exp))))

;; If we catch an exp in the form (:kibit.rules/vector ...) or (:kibit.rules/map ...)
;; we return the form converted back into its normal structure 
(def unwrap-struct-walker
  (partial walk/prewalk
           #(cond
            (kibit-vector? %)
            (apply vector (rest %))

            (kibit-map? %)
            (apply hash-map (rest %))
            
            :else %)))

(defn compile-rule [[pattern simpl]]
  (let [rule [(wrap-struct-walker pattern) (wrap-struct-walker simpl)]
        [pat alt] (logic/prep rule)]
     [(fn [expr] (logic/== expr pat))
      (fn [sbst] (logic/== sbst alt))]))

(defn raw-rule? [rule]
  (not (vector? rule)))

(defmacro defrules [name & rules]
  `(let [rules# (for [rule# '~rules]
                  (if (raw-rule? rule#)
                    rule# ;; raw rule, no need to compile
                    (compile-rule rule#)))]
     (def ~name (vec rules#))))
