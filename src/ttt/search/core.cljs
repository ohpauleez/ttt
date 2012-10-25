(ns ttt.search.core
  "Search and query all tickets in the system"
  (:require [ttt.core :as tttc]
            [ttt.search.protocols :as s-protocols]
            [clojure.walk :as walk])
  (:require-macros  [cljs.core.logic.macros :as m])
  (:use  [cljs.core.logic :only [membero prep
                                 binding-map* unifier*
                                 partial-map]]))

;; Martin Trojer is a solid practioner of all things core.logic.
;; I've learned a lot from his [blog](http://martinsprogrammingblog.blogspot.com)
;;
;; The query capabilities here are from one of his [Gists](https://gist.github.com/3122375)
;; Which he later [blogged about](http://martinsprogrammingblog.blogspot.com/2012/07/replicating-datomicdatalog-queries-with.html)

;; We're shooting to mimic Datomic as much as possible in query capabilites (and auto-joining)
;;
;;     (q '[:find ?first ?height
;;          :in [[?last ?first ?email]] [[?email ?height]]]
;;        [["Doe" "John" "jdoe@example.com"]
;;         ["Doe" "Jane" "jane@example.com"]]
;;        [["jane@example.com" 73]
;;         ["jdoe@example.com" 71]])

;; What Martin presents is a simpler case, where joins need to be done by hand
;;
;;      (query '(?last ?first) [["Doe" "John"]])
;;      (query-seq '(?first)  ["ole" "dole" "doff"])])
;;
;; And later he sets up a macro, `defquery` to do the auto joining with a list of facts

(defn query [rule xs]
  (let [prule (prep rule)]
    (set (map #(binding-map* prule (prep %)) xs))))
;(println (query '(?last ?first) [["Doe" "John"]]))

(defn query-seq
  "Wraps each member of a collection in vector before calling query"
  [rule xs]
  (query rule (map vector xs)))

(defn join-query
  "Unify the results of something from a query with another rule"
  [rule results]
  (let [prule (prep rule)]
    (map #(unifier* prule (prep %)) results)))

;; A standard query though will deal with ticket maps...
;;
;;      [?x :where [?x :owner "paul"]]
;;      [?x :point #"[0-2]H"] ; assumed shorthand
;;      [(:point ?x) :where [?x :owner "paul"]]

(defn normalize-query-vec [query-vec]
  (let [[target where & q-clauses] query-vec]
    (if (and (= where :where) (every? vector? q-clauses))
      query-vec
      (vector target :where query-vec))))
 
;; The logic version might do something like:
;; `[?x :where [(= (:owner ?x) "paul")] (all-tickets)]` => (->> (all-tickets) (query ?x) (filter #(= (:owner '?x) "paul")))
;; `[(?x :id) :where [?x :owner "paul"] (all-tickets)]` => (->> (all-tickets) (query ?x) (filter #(= (:owner (get % '?x)) "paul")) (map #(select-keys (get % '?x) [:id])))
;; `[[{:keys [:id]) ?x] :where [?x :owner "paul"][?x :owner "bob"] (all-tickets)]` => (->> (all- tickets) (query ?x) (filter #(or (= ) (=))
;; We can pre-comp all the filters together to speed things up (and this will make supporting "or" easier)
;; We can also still use search-compare which will eliminate the need for the hard-coded "="

;(defn build-form
;  "Build a form to ala (f-sym item1 item2 item3 ...)
;  given the items within a collection."
;  [f-sym coll]
;  (reverse (into (list f-sym) coll)))
;
;;; This version supports: [?x :where [(= (?x :owner) "paul")]]
;(defn logic-query [query-vec]
;  (let [query-vec (normalize-query-vec query-vec)
;        [target _ & q-clauses] query-vec
;        let-target (if (vector? target) target nil)
;        target-sym (if (sequential? target) nil target)
;        filter-clauses (walk/postwalk-replace {target-sym `(get % ~target-sym)}
;                         (reverse (into '(or)
;                                        (map #(build-form 'and (vec %)) q-clauses))))
;        tickets (:tickets (tttc/ticket-file!))]
;    (->> tickets (query target) (filter #(eval filter-clauses)))))

;; Another approach is to use partial-map unification, added recently by Kevin Lynagh

;(defn partial-map-query [query-vec]
;  (m/run* [q]
;    (m/fresh [pm x]
;             (m/== pm (partial-map {:a x}))
;             (m/== pm {:a 1 :b 2})
;             (m/== pm q))))



;; The basic vserion of this can use a comprehension and regex
;; `[?x :where [?x :owner "paul"]]` => `(for [t tickets :when (re-find #"paul" (:owner t)]) t)`

;; To support an open system for extensions, search comparisons are made
;; via a protocol, ISearchable, that defines a `search-compare`

(defn ensure-kw
  "If something is a string, and it looks like a keyword, but isn't a keyword,
  make it keyword, otherwise, pass-through"
  [kw]
  (if (and (string? kw) (= ":" (first kw)) (not (keyword? kw)))
    (keyword kw)
    kw))

(extend-protocol s-protocols/ISearchable

  js/String
  (-search-compare [this compare-to]
    (if (keyword? this) ;; CLJS interns keywords as strings...
      (or (= this compare-to)
          (re-find (js/RegExp. (name this)) compare-to))
      (re-find (js/RegExp. this) (name compare-to))))

  js/RegExp
  (-search-compare [this compare-to]
    (re-find this (name compare-to)))

  js/Number
  (-search-compare [this compare-to]
    (= this compare-to)))

(defn search-compare [t compare-to]
  (if (satisfies? s-protocols/ISearchable t)
    (s-protocols/-search-compare t compare-to)
    (= t compare-to)))

(defn query-tickets [query-vec]
  (let [query-vec (normalize-query-vec query-vec)
        [target _ q-clause] query-vec
        target (if (seq? target) target (list target))
        [q-target & kvs] q-clause
        kvs (partition 2 kvs)
        {ks true target-var false} (group-by keyword? target)
        ;;target-var (first target-var) ;; this is only needed for logic stuff
        tickets (:tickets (tttc/ticket-file!))]
    (for [t tickets
          :when (every? identity
                        (map (fn [[k v]] (search-compare (ensure-kw v) ((ensure-kw k) t))) kvs))]
      (if (empty? ks)
        t
        (select-keys t ks)))))

