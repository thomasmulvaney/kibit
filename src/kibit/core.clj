(ns kibit.core
  "Kibit's core functionality uses core.logic to construct idiomatic
   replacements/simplifications for patterns of code."
  (:require [clojure.walk :as walk]
            [kibit.rules.util :as util]
            [clojure.core.logic :as logic]))

;; ### Important notes
;; Feel free to contribute rules to [kibit's github repo](https://github.com/jonase/kibit)

;; Building an alternative form
;; ----------------------------
;;
;; ### Applying unification
;;
;; Performs the first simplification found in the rules. If no rules
;; apply the original expression is returned. Does not look at
;; subforms.

(defn simplify-one [expr rules]
  (let [wrapped-expr (util/wrap-struct-walker expr)
        alts (logic/run* [q]
               (logic/fresh [pat subst]
                 (logic/membero [pat subst] rules)
                 (logic/project [pat subst]
                   (logic/all (pat wrapped-expr)
                              (subst q)))))]
    
    (if (empty? alts) 
      expr
      (util/unwrap-struct-walker (first alts)))))

;; Simplifies expr according to the rules until no more rules apply.
(defn simplify [expr rules]
  (->> expr
       (iterate (partial walk/prewalk #(simplify-one % rules)))
       (partition 2 1)
       (drop-while #(apply not= %))
       (ffirst)))
