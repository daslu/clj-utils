(ns ^{:doc "General utility functions."}
  clj-utils.misc
  (:use (incanter core stats io datasets))
  (:require clojure.java.shell)
  (:require clj-time.core)
  (:require clj-time.coerce)
  ;;(:require cemerick.pomegranate)
  )

;;;;
;; Parallelization with a fixed number of threads.
;; See mikera's answer here:
;; http://stackoverflow.com/a/10974227
(defn split-equally [num coll] 
  "Split a collection into a vector of (as close as possible) equally sized parts"
  (loop [num num 
         parts []
         coll coll
         c (count coll)]
    (if (<= num 0)
      parts
      (let [t (quot (+ c num -1) num)]
        (recur (dec num) (conj parts (take t coll)) (drop t coll) (- c t)))))) 
(defmacro dopar [thread-count [sym coll] & body]
 `(doall (pmap 
    (fn [vals#]
      (doseq [~sym vals#]
        ~@body))  
    (split-equally ~thread-count ~coll))))
;; my test
(defn test-dopar [thread-count]
  (dopar
    thread-count
    [x (range 99)] 
    (let [thread-id (.getId (java.lang.Thread/currentThread))]
      (java.lang.Thread/sleep (* 1000 (rand)))
      (println (str x ": "
                 "\tthreadid="
                 thread-id
                 "\tactivecount="
                 (java.lang.Thread/activeCount)
                 ))
      (java.lang.Thread/sleep (* 1000 (rand))))))
;;;;


;;;;
;; measuring time of execution
;;
;; https://groups.google.com/forum/#!topic/clojure/bKBkInBCzf8
(defmacro bench
  "Times the execution of forms, discarding their output and returning
a long in nanoseconds."
  ([& forms]
    `(let [start# (System/nanoTime)]
       ~@forms
       (- (System/nanoTime) start#))))
;; my adjustment
(defmacro print-bench-and-return
  "Prints execution time of forms and returns their return value."
  ([title & forms]
     `(let [start# (System/nanoTime)
           return-val# ~@(doall forms)]
       (println ["bench"
                  ~title
                  (* (- (System/nanoTime) start#)
                     1.0E-9)
                  "sec"])
       return-val#)))
;;;;



;;;;
;;
;; writing an object to a temporary file
;;
(defn temp-pprint
  "Given an object, pretty-print it to a temporary file, then return the filename."
  [object]
  ;; Choose filename.
  (let [filename (str "/tmp/object_" (rand-int (Integer/MAX_VALUE)) ".clj")]
    ;; pprint to file.
    (spit filename (with-out-str (clojure.pprint/pprint object)))
    ;; return 
    filename))



;;;;
;;
;; comparing objects
;;
(defn diff-objects 
  "Given a sequence of objects, pretty-print each object to a temporary file,
invoke a chosen diff tool to compare these files, and then delete the files.
Try this with diff, meld, ediff, etc. as the tool."
  [tool objects]
  (let [
        ;; Create temporary files containing the pretty-printe objects.
        temp-filenames (map temp-pprint objects)]
    ;; Invoke the diff tool.
    (def sh-result (apply clojure.java.shell/sh
                          (cons tool temp-filenames)))
    ;; Remove the files.
    (doseq [filename temp-filenames]
      (clojure.java.shell/sh "rm" filename))
    ;; Return the result returned by the diff tool.
    sh-result))



(defn nested-sorted-map [m]
    (apply sorted-map
         (apply concat (for [[k v] m]
                    (if (map? v)
                      [k (nested-sorted-map v)]
                      [k v])))))



(defn only-one [s]
  (do (if (< 1 (count s))
        (throw (RuntimeException. (str (count s) " is too many: s=" s)))
        ;; else -- only one element -- return it
        (first s))))



(defn rmse
  "Compute the root mean square error between seqables x, y."
  [x y]
  (Math/sqrt (reduce + (map #(* % %)
                            (map - x y)))))


(defn is-a-number
  "Check if object x is non-nil and can be cast to a Double other than NaN."
  ;; TODO: Handle general types.
   [x]
  (and (identity x)
       (not (Double/isNaN x))))

(defn filter-complete-rows
  "Given a dataset, leave only the rows all of whose elements are numbers."
   [adataset]
   (let [num-cols (count (col-names adataset))]
     (->> adataset
          :rows
          (filter #(= (count %)
                      num-cols))
          (filter (comp (partial every? is-a-number) vals))
          (dataset (col-names adataset))
          ;; col-names are necessary here to keep the ordering of row columns.
          )))

(defn take-all-but
  "Take all elements of seqable x (of finite count) except for the last n elements."
   [n x]
  (take (- (count x)
           n)
        x))

(defn freqs-as-rows [x]
  "Return the frequencies of x as a sequence of maps
 (which can be thought of as rows of a dataset)."
  (map #(hash-map :val (first %) :count (second %))
       (sort-by (comp  - second) (frequencies x))))


(defn concat-with-delimiter
  "Concat the String representations of given objscts with a given delimiter.
For example: (concat-with-delimiter \",\" (range 3)) ==> \"0,1,2\""
  [delimiter objects]
  (apply str
         (rest (interleave (repeat delimiter)
                           objects))))


(defn round-to-unit-size [unit-size value]
  (* unit-size
     (quot value unit-size)))

(defn concat-keywords
  [& keywords]
  (keyword (apply str
                  (interleave (cons "" (repeat "-"))
                              (map name keywords)))))


(defn ohlc
  "Compute open,high,low,close values of given sequence xs of values. The resulting map's keywords' names will begin with key-prefix."
  [xs prefix-keyword]
  {(concat-keywords prefix-keyword :open) (first xs)
   (concat-keywords prefix-keyword :high) (apply max xs)
   (concat-keywords prefix-keyword :low) (apply min xs)
   (concat-keywords prefix-keyword :close) (last xs)})

(defn identity-map [aseq]
  (apply hash-map (interleave aseq aseq)))


(defn construct-index [keys]
  (apply hash-map
         (interleave keys (range))))


(defn log-ratio [x y]
  (java.lang.Math/log (/ x y)))


(defn nil-to-nan [x]
  (if x x (Double/NaN)))

(defn nil-to-zero [x]
  (if x x (Double/NaN)))


(defn signum [x]
  (if (pos? x)
    1
    (if (neg? x)
      -1
      nil)))

(defn transform-keys
  "Transform the keys of map m by function f."
  [f m]
  (apply hash-map
         (apply concat
                (for [[k v] m]
                  [(f k) v]))))

(comment
  (= (transform-keys inc {3 5 2 9})
     {4 5 3 9}))



;; ;;;;
;; ;;
;; ;; Adding jars to classpath in REPL 
;; ;;
;; (defn add-to-cp
;;   "Add jar to classpath in repl.
;; See https://groups.google.com/forum/?fromgroups#!topic/clojure/AJXqbpGMQw4"
;;   [#^String jarpath]
;;   (let [#^java.net.URL url (.. (java.io.File. jarpath) toURI toURL)
;;        url-ldr-cls (. (java.net.URLClassLoader. (into-array java.net.URL [])) getClass)
;;        arr-cls (into-array Class [(. url getClass)])
;;        arr-obj (into-array Object [url])
;;        #^java.lang.reflect.Method mthd (. url-ldr-cls getDeclaredMethod "addURL" arr-cls)]
;;    (doto mthd
;;      (.setAccessible true)
;;      (.invoke (ClassLoader/getSystemClassLoader) arr-obj))
;;    (println (format "Added %s to classpath" jarpath))))

;; (defn add-to-cp-and-println
;;   "Dynamically add a given jar to the classpath, mentioning it by println."
;;   [jar]
;;   (do
;;     (println (str "adding " jar))
;;     (cemerick.pomegranate/add-classpath jar)))
;; ;;;;




;; redirecting output to a file
;; http://briancarper.net/blog/495/
(defmacro redir
  [filename & body]
  `(with-open [w# (clojure.java.io/writer ~filename)]
          (binding [*err* w#
                    *out* w#] ~@body)))



(defn get-current-time-string []
  "Get time as a string suitable for filenames."
  (clojure.string/replace 
                 (clj-time.coerce/to-string
                  (clj-time.core/now))
                 #":" ""))


(defn in?
  "Is the e one of the elements in es?"
  [e es]
  (some #(= e %) es))


(defn disjoint?
  "Are two seqables given disjoint?"
  [s1 s2]
  (not (some (apply hash-set s1)
             s2)))

