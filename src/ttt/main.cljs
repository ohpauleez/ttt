(ns ttt.main
  (:require [ttt.core :as core]
            [ttt.api :as api]
            [ttt.cli :as cli]
            [ttt.search.core :as search]
            [goog.result :as result]))

(defn ticket-arg-handler [args]
  (cli/cli args
           ["-m" "--message" "The title message for the ticket"]
           ["-t" "--type" "The type of the ticket"] ; This default should be set with the config
           ["-r" "--release" "The release to which this ticket belongs" :parse-fn keyword]
           ["-p" "--points" "The number of points for this ticket"] ; This default should be set with the config
           ["-s" "--sub" "Place this ticket as a subticket to another ticket" :parse-fn core/id-str->int]
           ["-h" "--help" "See this usage/help information"]))

(defn main-control [git-conf args]
  (let [git-map (.getValue git-conf)
        pref-file (core/pref-file!)
        ticket-file (core/ticket-file!)
        ;new-ticket (core/ticket-io! core/append-ticket (core/make-ticket))      
        [arg-map extra-args usage] (ticket-arg-handler args)
        f-arg (first extra-args)]
    ;; Dispatch on the few special case primary args we have:
    ;;  * __core__ is for API calls
    ;;  * :[0-9] is for modifying/updating a ticket
    ;;  * st|status is a status lookup (for tickets, releases, progress, etc)
    ;;  * [...] is for searching/querying
    (cond
      (and (empty? arg-map) (empty? extra-args)) (println usage)
      (:help arg-map) (println usage)
      (re-find #":\d+" f-arg) (core/ticket-io! core/update-ticket
                                                   (apply hash-map
                                                          (core/scrub-ticket-args
                                                            (cli/into-arg [:id (core/id-str->int f-arg)]
                                                                          arg-map))))
      (#{"st" "status"} f-arg) (prn (search/query-tickets [?x :where
                                                              [?x :owner (:user.email git-map)
                                                                  :current-state (str "^(?!" (-> (:ticket-states pref-file) last) "$)")]]))
      (re-find #"^\[.+\]" f-arg) (prn (search/query-tickets (core/safe-read f-arg)))
      (= "__core__" f-arg) (prn (api/call-core-fn (cli/into-arg (rest extra-args)
                                                                (dissoc arg-map :help))))
      :else (core/ticket-io! core/append-ticket
                     (apply core/make-ticket (cli/into-arg extra-args
                                                           (dissoc arg-map :help)))))
    (core/exit)))

(defn -main [& args]
  (let [git-root core/git-root-res
        git-conf core/git-config-res]
    (result/wait git-root
      #(binding [core/*repo-root* (.getValue git-root)]
         (result/wait git-conf
           (fn [] (main-control git-conf args)))))))

(set! *main-cli-fn* -main)

