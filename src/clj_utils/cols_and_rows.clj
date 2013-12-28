(ns ^{:doc "Functions for working with cols-and-rows structures.
A cols-and-rows structure consists of column-names and a sequence of rows,
which arerepresented as maps from column-names to values.
It is intended to serve as a (possibly) analog counterpart of Incanter's dataset."}
  clj-utils.cols-and-rows
  (:use (incanter core datasets stats)))


(defn read-cols-and-rows [filename & {:keys [seq-transformer delimiter]
                                      :or {seq-transformer identity
                                           delimiter ","}}]
  (let [delimiter-pattern (re-pattern delimiter)
        file-reader (clojure.java.io/reader filename)
        column-names (->> (.readLine file-reader)
                          (#(clojure.string/split % delimiter-pattern))
                          (map keyword))
        ;; Note that the side effect of the last let-element
        ;; is that the file-reader has progressed to its 2nd
        ;; line.
        lines (seq-transformer (line-seq file-reader))
        rows-vals (map #(clojure.string/split % delimiter-pattern)
                       lines)
        ;;rows-vals (csv/read-csv file-reader)
        rows (map (fn [row-vals]
                    (apply hash-map
                           (interleave column-names row-vals)))
                  rows-vals
                  ;;(seq-transformer rows-vals)
                  )]
    (println ["reading" filename "with delimiter-pattern" (re-pattern delimiter)])
    {:column-names column-names
     :rows rows}))


(defn transform-cols-and-rows
  [new-columns-fns cols-and-rows]
  {:column-names (keys new-columns-fns)
   :rows (map
          (fn [row]
            (apply hash-map
                   (apply concat
                          (map (fn [column-name]
                                 [column-name ((column-name new-columns-fns)
                                               row)])
                               (keys new-columns-fns)))))
          (:rows cols-and-rows))})

(defn take-cols-and-rows
  [nrows cols-and-rows]
  {:column-names (:column-names cols-and-rows)
   :rows (take nrows (:rows cols-and-rows))})

(defn sample-from-cols-and-rows
  [size cols-and-rows]
  {:column-names (:column-names cols-and-rows)
   :rows (sample (:rows cols-and-rows)
                 :size size)})

(defn filter-cols-and-rows
  [row-filter-func cols-and-rows]
  {:column-names (:column-names cols-and-rows)
   :rows (filter row-filter-func (:rows cols-and-rows))})

(defn cols-and-rows-to-dataset
  [cols-and-rows]
  (dataset (:column-names cols-and-rows)
           (:rows cols-and-rows)))

(defn dataset-to-cols-and-rows
  [adataset]
  {:column-names (col-names adataset)
   :rows (:rows adataset)})

(defn sort-column-names-of-cols-and-rows
  [cols-and-rows]
  (assoc cols-and-rows
    :column-names (sort (:column-names cols-and-rows))))

(defn add-linear-combination-column
  [coeff-map new-col-name cols-and-rows]
  (let [coeff-vals (vals coeff-map)
        coeff-col-names (keys coeff-map)]
    {:column-names (cons new-col-name
                         (:column-names cols-and-rows))
     :rows (for [row (:rows cols-and-rows)]
             (let [relevant-vals (map row coeff-col-names)
                   lincomb (if (every? identity relevant-vals)
                             (apply
                              +
                              (map *
                                   relevant-vals
                                   coeff-vals)))]
               (assoc row
                 new-col-name lincomb)))}))

(defn remove-columns
  [col-names-to-remove cols-and-rows]
  {:column-names (filter (complement (set col-names-to-remove))
                         (:column-names cols-and-rows))
   :rows (for [row (:rows cols-and-rows)]
           (apply dissoc row col-names-to-remove))})


(defn replace-columns-by-linear-combination
  [coeff-map new-col-name cols-and-rows]
  (remove-columns (keys coeff-map)
                  (add-linear-combination-column coeff-map
                                                 new-col-name
                                                 cols-and-rows)))
