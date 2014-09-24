(ns kibit.driver
  (:require [clojure.java.io :as io]
            [kibit.check :refer [check-file]]
            [kibit.reporters :refer :all]
            [clojure.tools.cli :refer [cli]]))

(def cli-specs [["-r" "--reporter"
                 "The reporter used when rendering suggestions"
                 :default "text"]])

;; Thanks @clinton
(defn find-clojure-sources-in-dir [dir]
  (letfn [(clojure-source-file? [file]
            (and (.isFile file)
                 (let [name (.getName file)]
                   (or (.endsWith name ".clj")
                       (.endsWith name ".cljs")))))]

    (sort-by #(.getAbsolutePath %)
             (filter clojure-source-file? (file-seq dir)))))

(defn run [source-paths & args]
  (let [[options file-args usage-text] (apply (partial cli args) cli-specs)
        source-files (if (empty? file-args)
                       (mapcat #(-> % io/file find-clojure-sources-in-dir)
                               source-paths)
                       file-args)]
    (doseq [file source-files]
      (try (check-file file :reporter (name-to-reporter (:reporter options)
                                                        cli-reporter))
           (catch Exception e
             (println "Check failed -- skipping rest of file")
             (println (.getMessage e)))))))
