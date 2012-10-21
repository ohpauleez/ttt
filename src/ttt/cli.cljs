(ns ttt.cli
  "A port of clojure.tools.cli that works in Paul's CLJS"
  (:require [clojure.string :as cstr]))

;; TODO
;; This needs some serious overhauling
;; it should support: -m something -m"something" --message something and --message=something


(defn- build-doc [{:keys [switches docs default]}]
  [(cstr/join ", " switches)
   (or (str default) "")
   (or docs "")])

(defn- banner-for [specs]
  (prn "Usage:\n")
  (let [docs (into (map build-doc specs)
                   [["--------" "-------" "----"]
                    ["Switches" "Default" "Desc"]])
        max-cols (->> (for [d docs] (map count d))
                      (apply map (fn [& c] (apply vector c)))
                      (map #(apply max %)))
        vs (for [d docs]
             (mapcat (fn [& x] (apply vector x)) max-cols d))]
    (doseq [v vs]
      (prn v))))

(defn- name-for [k]
  (cstr/replace k #"^--no-|^--\[no-\]|^--|^-" ""))

(defn- flag-for [^String v]
  ;; indexOf scans the whole string, which sucks, but `subs` and throw an exception
  ;; so alas, we're left with trading some performance for portable
  (not (= 0 (.indexOf  v "--no-"))))

(defn- opt? [^String x]
  (= 0 (.indexOf x "-")))

(defn- flag? [^String x]
  (= 0 (.indexOf x "--[no-]")))

(defn- end-of-args? [x]
  (= "--" x))

(defn- spec-for
  [arg specs]
  (->> specs
       (filter (fn [s]
                   (let [switches (set (s :switches))]
                     (contains? switches arg))))
       first))

(defn- default-values-for
  [specs]
  (reduce (fn [m s]
            (if (contains? s :default)
              (assoc m (:name s) (:default s))
              m))
          {} specs))

(defn- apply-specs
  [specs args]
  (loop [options    (default-values-for specs)
         extra-args []
         args       args]
    (if-not (seq args)
      [options extra-args]
      (let [opt  (first args)
            spec (spec-for opt specs)]
        (cond
         (end-of-args? opt)
         (recur options (into extra-args (vec (rest args))) nil)

         (and (opt? opt) (nil? spec))
         (throw (str "'" opt "' is not a valid argument"))
         
         (and (opt? opt) (spec :flag))
         (recur (assoc options (spec :name) (flag-for opt))
                extra-args
                (rest args))

         (opt? opt)
         (recur (assoc options (spec :name) ((spec :parse-fn) (second args)))
                extra-args
                (drop 2 args))

         :default
         (recur options (conj extra-args (first args)) (rest args)))))))

(defn- switches-for
  [switches flag]
  (-> (for [^String s switches]
        (cond
         (and flag (flag? s))            [(cstr/replace s #"\[no-\]" "no-") (cstr/replace s #"\[no-\]" "")]
         (and flag (.startsWith s "--")) [(cstr/replace s #"--" "--no-") s]
         :default                        [s]))
      flatten))

(defn- generate-spec
  [raw-spec]
  (let [[switches raw-spec] (split-with #(and (string? %) (opt? %)) raw-spec)
        [docs raw-spec]     (split-with string? raw-spec)
        options             (apply hash-map raw-spec)
        aliases             (map name-for switches)
        flag                (or (flag? (last switches)) (options :flag))]
    (merge {:switches (switches-for switches flag)
            :docs     (first docs)
            :aliases  (set aliases)
            :name     (keyword (last aliases))
            :parse-fn identity
            :flag     flag}
           (when flag {:default false})
           options)))

;(def with-out-str prn)

(defn cli
  "Parse the provided args using the given specs. Specs are vectors
  describing a command line argument. For example:

  [\"-p\" \"--port\" \"Port to listen on\" :default 3000 :parse-fn #(Integer/parseInt %)]

  First provide the switches (from least to most specific), then a doc
  string, and pairs of options.

  Valid options are :default, :parse-fn, and :flag. See
  https://github.com/clojure/tools.cli/blob/master/README.md for more
  detailed examples.

  Returns a vector containing a map of the parsed arguments, a vector
  of extra arguments that did not match known switches, and a
  documentation banner to provide usage instructions."
  [args & specs]
  (let [specs (map generate-spec specs)]
    (let [[options extra-args] (apply-specs specs args)
          banner  (with-out-str (banner-for specs))]
      [options extra-args banner])))

(defn into-arg [extra-args arg-map]
  (flatten (into (vec extra-args) arg-map)))

