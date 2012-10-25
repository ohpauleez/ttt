(ns ttt.core
  "Core functionality: initialization, ticket creation, data operations"
  (:require [clojure.string :as cstr]
            [cljs.nodejs :as node]
            [cljs.reader :as reader]
            [goog.result :as result]))

;; Node modules
;; -------------
(def ^:private fs (node/require "fs"))
(def ^:private sys-exec (.-exec (node/require "child_process")))

(def ^:dynamic *repo-root* "./")

;; Base maps
;; ------------
(def base-pref-map
  {:types ["work" "bug"]
   :default-type "work"
   :points ["0" "1" "2" "3" "5" "8"]
   :default-ticket-points "0"
   :tag-on-complete? true
   :ticket-states ["open" "in-progress" "closed"] ;; these get kw-ized within ttt
   })

(def base-file-map
  {:releases {}
   :tickets []})

(def base-ticket-map
  {:summary ""
   :description ""
   :reported-by ""
   :owner ""
   :closed-by ""
   :points ""
   :type ""
   :states {}
   :current-state :open
   :history []
   :branch ""})

(def default-ticket-path (str *repo-root* "/tickets.edn"))

;; Core I/O functions
;; ------------------
;;
;; ### Safe Reading
;; Usually `safe-read` is defined as such:
;;
;;     (defn safe-read [s]
;;       (binding [*read-eval* false]
;;         (reader/read-string s)) 
;;
;; But ClojureScript doesn't have eval, so we get the simpler...
(defn safe-read [s]
  (reader/read-string s))

(defn read-edn-file
  "Read in a Clojure/EDN file, returning the data structure upon success.
  Throws an error if reading fails"
  [edn-file]
  (let [file-contents (.readFileSync fs edn-file "utf8")]
    (safe-read file-contents)))

(defn write-edn-file
  "Writes Clojure/EDN data to a file. Returns the data upon success.
  Throws an error when writing fails"
  [edn-file data]
  (let [file-contents (pr-str data)]
    (.writeFileSync fs edn-file file-contents)
    data))

(defn fs-exists? [fs-path]
  (.existsSync fs fs-path))

(defn file? [fs-path]
  (if (fs-exists? fs-path)
    (-> (.lstatSync fs fs-path) .isFile)
    false))

(defn directory? [fs-path]
  (if (fs-exists? fs-path)
    (-> (.lstatSync fs fs-path) .isDirectory)
    false))

;; System functions
;; ----------------
(defn exit
  ([]
   (exit 0))
  ([exit-code]
   (node/process.exit exit-code)))

;; Git functions
;; --------------
;;
;; ### The story
;; Node only allows for async command execution.
;; This quickly becomes a pain in an application like `ttt`.
;;
;; To get around this, we use Closure's `Result` - essentially a promise.
;; When coupled with an atom, we can perform system commands and await their
;; return using a blocking deference.  The trade-off is that we still need to
;; pass callbacks to the promise but at the call site (for example, in the
;; `main` namespace) where they have real context.

(defn git-config
  "Call the `git config` command to grab the value of a given key.
  Pass in a callback to handle the output/return string"
  [config-key callback]
  (sys-exec (str "git config --get " k)
              (fn [_ out _]
                (callback (cstr/trim out)))))

(defn git-config-atom
  "Call the `git config` command to grab the value of a given key.
  Return an atom that will hold the return/output string in a map {k output-value}.
  Optionally pass in an atom (allowing you to build up many returns)"
  ([k]
   (git-config-atom k (atom {})))
  ([k ret]
   (do (sys-exec (str "git config --get " (name k))
                 (fn [_ out _]
                   (swap! ret assoc (keyword k) (cstr/trim out))))
   ret)))

(defn git-root
  "Fetch the root directory of a git repo;  This relies on the `root` alias"
  []
  (let [ret (atom nil)]
    (sys-exec "git root"
              (fn [_ out _]
                (reset! ret (cstr/trim out))))
    ret))

(defn blocking-deref
  "Given an atom and a Result object, attempt to cycle the process tick on derefing until `pred-fn` isn't true
  By default, `pred-fn` is nil?
  Once the condition doesn't hold, the Result will be set to @a, and @a is returned"
  ([a r]
   (blocking-deref a r nil?))
  ([a r pred-fn]
   (if (pred-fn @a)
     (node/process.nextTick #(blocking-deref a r pred-fn))
     (do (.setValue r @a)
       @a))))

;; This is the direct Result object you can use to access
;; a git config map
(def git-config-res
  (let [res (goog.result.SimpleResult.)]
    (->>
      (git-config-atom "user.email")
      (git-config-atom "user.name")
      ((fn [a] (blocking-deref a res #(< (count %) 2)))))
    res))

;;This is the direct Result object you can use to access
;; the git repo's root directory
(def git-root-res
  (let [res (goog.result.SimpleResult.)]
    (->
      (git-root)
      (blocking-deref res))
    res))

;; Lock file I/O
;; --------------
;; The lock file ensures that only one command is modifying the ticket file
;; at any given point in time.  This is similar to Vim's .swp file.

(defn lock-file! []
  (let [l-file (str *repo-root* "/.tttlock")]
    (if (file? l-file)
      (do (println "TTT is currently locked.  Is someone else trying running ttt commands?\n")
        (exit 1))
      (write-edn-file l-file ""))))

(defn unlock! []
  (let [l-file (str *repo-root* "/.tttlock")]
    (when (file? l-file)
      (.unlinkSync fs l-file)
      true)))

;; Preferences specific I/O
;; ------------------------
(defn pref-file! []
  (let [p-dir (str *repo-root* "/.ttt/")
        p-file (str p-dir "tttrc")]
    (if-not (fs-exists? p-file)
      (do (when-not (directory? p-dir)
            (.mkdirSync fs p-dir))
        (write-edn-file p-file base-pref-map))
      (read-edn-file p-file))))

;; Ticket Store/File specific I/O
;; ------------------
(defn write-ticket-file
  [ticket-file ticket-map]
  (write-edn-file ticket-file ticket-map))

(defn read-ticket-file
  [ticket-file]
  (read-edn-file ticket-file))

(defn ticket-file!
  "Ensure the ticket file exists, and return its contents as a map.
  If the ticket files does not exist, create it, and return the base map"
  ([]
   (ticket-file! default-ticket-path))
  ([t-file]
   (if-not (file? t-file)
     (write-ticket-file t-file base-file-map)
     (read-ticket-file t-file))))

(defn utc-millis []
  (let [d (js/Date.)]
    (js/Date.UTC (.getUTCFullYear d) (.getUTCMonth d) (.getUTCDay d))))

;; Ticket auxiliary
;;--------------------
;;
;; A map is used to normalize the arguments used in creating and modifying
;; tickets.  This decouples the handling of CLI args and the args passed to
;; the tickets.  While the basic substituion could be done with
;; clojure.set/rename-keys, this offers an open and composible solution
;; for external plugins as well.

(def normalize-ticket-arg
  {"-m" :summary
   "--message" :summary
   :message :summary
   "-d" :description
   "--description" :description
   "-p" :points
   "--points" :points
   "-t" :type
   "--type" :type
   "-r" :release
   "--release" :release
   "-RR" :release
   "--release*" :release})

(defn scrub-ticket-args [args]
  (map #(get normalize-ticket-arg % %) args))

(defn ticket-defaults
  "This should only be used on ticket creation"
  []
  (let [pref-file (pref-file!)
        id-num (-> (ticket-file!) :tickets count inc)
        start-state (-> pref-file :ticket-states first keyword)
        git-map (.getValue git-config-res)]
    {:id id-num
     :type (:default-type pref-file)
     :current-state start-state
     :points (get pref-file :default-ticket-points "")
     :states {start-state (utc-millis)}
     :reported-by (:user.email git-map)
     :owner (:user.email git-map)}))

(defn inc-state [ticket]
  (let [pref-file (pref-file!)
        curr-state-str (-> ticket :current-state name)
        ticket-states (:ticket-states pref-file)]
    (if-let [next-state-str (second (drop-while (complement #{curr-state-str}) ticket-states))]
      (merge ticket {:current-state (keyword next-state-str)
                     :states (assoc (:states ticket) (keyword next-state-str) (utc-millis))})
      ticket)))

(defn inc-state-on
  "Inc the state of a ticket only if the ticket's current state is a specific keyword"
  [ticket state-kw]
  (if (= state-kw (:current-state ticket))
    (inc-state ticket)
    ticket))

(defn id->pos
  "Given a ticket's integer ID, return its supposed position in the ticket vector
  This is naive, but gets the job done."
  [id-int]
  (dec id-int))

(defn id-str->int
  "Given a ticket's ID string expressed like a keyword (ie: ':1'), return the integer (1)"
  [id-str]
  (js/parseInt (subs id-str 1)))

;; Ticket specific I/O
;;--------------------
(defn make-ticket [& args]
  (let [ticket (merge base-ticket-map
                      (ticket-defaults)
                      (apply hash-map (scrub-ticket-args args)))]
    ;; We may have some hooks here, if not TODO remove the `let`
    ticket))

(defn append-ticket
  "Append a given ticket to the ticket vector/collection"
  [t-file-map ticket]
  (update-in t-file-map [:tickets] conj (assoc ticket
                                               :id (-> t-file-map :tickets count inc))))

(defn update-ticket
  "Update a ticket within the ticket vector/collection.
  The ticket passed must already have an :id"
  [t-file-map ticket]
  (update-in t-file-map [:tickets (-> ticket :id id->pos)] merge ticket))

(defn ticket-io!
  "This applies a function to the actual ticket file.
  All functions should take two args: the ticket file map and a ticket.
  You should never lock the ticket file by hand, use this function."
  ([f ticket]
   (ticket-io! f ticket default-ticket-path))
  ([f ticket ticket-file]
   (let [l-file (lock-file!)
         t-file-map (ticket-file! ticket-file)
         res (write-ticket-file ticket-file (f t-file-map ticket))
         _ (unlock!)]
     res)))

;; Ticket formatting and printing
;; ------------------------------

;; Create a short hand to determine the opening and closing state of a ticket
(defn open-state [pref-file]
  (-> (:ticket-states pref-file) first keyword))
(defn closed-state [pref-file]
  (-> (:ticket-states pref-file) last keyword))

(defn str-ticket
  "A basic single-line str for a ticket; suitable to print"
  [t]
  (let [pref-file (pref-file!)
        status-map {(open-state pref-file) "_ "
                    (closed-state pref-file) "X "}]
    (str (get status-map (:current-state t) "> ") (:id t) " [" (:type t) " :: " (:points t) " points] - " (:summary t))))

(def millis-per-day 86400000)
(defn str-ticket-long
  "A full format ticket string; suitable to report on a single ticket"
  [t]
  (let [pref-file (pref-file!)
        open? (-> (:states t) ((open-state pref-file)))
        days-opened (or (and open? (-> open? (#(- (utc-millis) %)) (/ millis-per-day)))
                        "N/A")
        work?  (-> (:history t) last :at)
        days-since (or (and work? (-> work? (#(- (utc-millis) %)) (/ millis-per-day)))
                       days-opened)]
    (if t
      (str "Issue " (:id t) " - " (:summary t)
           "\n------------------------"
           "\n Description: " (:description t)
           "\n Type: " (:type t)
           "\n Status: " (:current-state t)
           "\n Points: " (:points t)
           "\n Reported by: " (:reported-by t) " - " days-opened " day(s) ago"
           "\n Owned by: " (:owner t)
           "\n Last activity: " days-since " day(s) ago"
           "\n On branch: " (:branch t)
           "\n Release: " (:release t))
      "Could not locate that ticket")))

;; Git interactions
;; -----------------
;;
;; Here are the auxiliary functions for various git hooks and interactions.
;; These are responsible for automatically pushing a ticket through its states
;; based on git usage.

(defn process-work
  "Process a given unit of work for a ticket, given the ticket,
  a commit hash, a possible command/state, and any extra args
  to merge into the history map"
  [ticket git-hash command & extra-args]
  (let [pref-file (pref-file!)
        curr-time (utc-millis)
        ticket (update-in ticket [:history] conj (merge {:commit git-hash :at curr-time} (apply hash-map extra-args)))]
    (if-let [state-str (some #{command} (:ticket-states pref-file))]
      (let [ticket (assoc (assoc-in ticket [:states (keyword state-str)] curr-time)
                          :current-state (keyword state-str))]
        (if (= state-str (last (:ticket-states pref-file)))
          (assoc ticket :closed-by (:owner ticket))
          ticket))
      ticket)))

(defn parse-git-message
  "Given a commit message of the form: 'this is my commit [...]'
  break it apart into a map of aossciated ticket ids, potential ttt commands,
  and the raw commit message itself"
  [message-str]
  (if-let [hook-str (re-find #"\[.+]$" message-str)]
    (let [hook-sans-br (cstr/replace hook-str #"\[|\]" "")
          parts (cstr/split hook-sans-br #"\s")
          command (first parts) ;; Note, this could not be a command, but just the mention of a ticket
          ticket-ids (map id-str->int (filter #(= \:  (get % 0)) parts))]
      {:ids ticket-ids :maybe-command command :message message-str})))

(defn process-git
  "This is the entry point for processing git feedback.
  It takes a commit hash, the author email, and any chunks of the message.
  These are most easily obtained with: `git log -n1 --pretty=format:'%H %ae %s'`"
  [git-hash email & message-pieces]
  (let [message-map (parse-git-message (cstr/join " " message-pieces))
        command (:maybe-command message-map)
        tickets (:tickets (ticket-file!))]
    (doseq [ticket-id (:ids message-map)]
      (when-let [ticket (get tickets (id->pos ticket-id))]
        (ticket-io! update-ticket (-> (inc-state-on ticket :open)
                                    (assoc :owner email)
                                    (process-work git-hash command :message (:message message-map))))))))

