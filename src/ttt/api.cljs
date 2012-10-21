(ns ttt.api
  "Handle all operations and dispatching for the CLI-driven API"
  (:require [clojure.string :as cstr]
            [ttt.core :as tttc]))

;; CLJS has no real notion of namespeaces, so this doesn't work:
;;
;;     (defn call-core-fn [cmd-args]
;;       (let [[fn-str & args] cmd-args]
;;         (apply (ns-resolve tttc (symbol fn-str)) args)))

; Instead we rely upon a closed `exports` like system and use a dispatch table
; TODO: this system has to be open to extension to appropriately support plugins
(def api-map
  {"read-edn-file" tttc/read-edn-file
   "write-edn-file" tttc/write-edn-file
   "lock-file!" tttc/lock-file!
   "unlock!" tttc/unlock!
   "pref-file!" tttc/pref-file!
   "ticket-file!" tttc/ticket-file!
   "write-ticket-file" tttc/write-ticket-file
   "read-ticket-file" tttc/read-ticket-file
   "utc-millis" tttc/utc-millis
   "inc-state" tttc/inc-state
   "id->pos" tttc/id->pos
   "make-ticket" tttc/make-ticket
   "append-ticket" tttc/append-ticket
   "update-ticket" tttc/update-ticket
   "ticket-io!" tttc/ticket-io!
   "parse-git-message" tttc/parse-git-message
   "process-git" tttc/process-git
   "arg-identity" identity})

(defn call-core-fn [cmd-args]
  (let [[fn-str & args] cmd-args]
    ;; If we're doing ticket-io, the second arg is a function that we need to resolve
    (if (= fn-str "ticket-io!")
      ((get api-map fn-str) (get api-map (first args) identity) (tttc/safe-read (second args)))
      (apply (get api-map fn-str identity) args))))

