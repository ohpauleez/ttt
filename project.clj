(defproject ttt "0.1.0-SNAPSHOT"
  :description "git-backed text-based ticket tracker"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/core.logic "0.8.0-beta1"]]
  :plugins [[lein-cljsbuild "0.2.9"]
             [jonase/kibit "0.0.4"]
             [lein-catnip "0.4.1"]]

  ;; This project current needs patches which are not landed in CLJS master
  ;; These settings are here for future reference.
  ;; Please use the `compile` script
  :cljsbuild {:builds [{:source-path "src",
                        :compiler {:output-dir "build/cljs",
                                   :output-to "build/ttt",
                                   :optimizations :advanced,;:advanced ;:simple ;:whitespace
                                   :pretty-print false
                                   :target :nodejs}}]} 
  :warn-on-reflection false)

