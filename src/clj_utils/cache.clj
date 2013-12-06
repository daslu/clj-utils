(ns ^{:doc "Functions related to caching results of computations."}
  clj-utils.cache
  (:use clj-utils.misc)
  (:use [clojure.java.shell :only [sh]]))


(defn compute-and-save-or-load [compute-output-fn
                                construct-filename-for-input-fn
                                read-fn
                                write-fn
                                input
                                title]
  (do ["called computation for" title]
    (print-bench-and-return
     title
     (try (let [filename (construct-filename-for-input-fn input)]
            (if (not (.exists (java.io.File. filename)))
              (let [output (compute-output-fn input)]
                (println ["class of output:" (class output)])
                (write-fn filename output)
                (println ["wrote" filename])
                output)
              ;; else 
              (do
                (println ["reading" filename])
                (read-fn filename))))
          (catch Exception e
            (do
              (println (str "got Exception "
                            (.getMessage e)
                            " for input "
                            input))
              (.printStackTrace e)))))))


