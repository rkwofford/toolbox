(defproject toolbox "0.1.0-SNAPSHOT"
  :description "Toolbox"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [incanter "1.9.0"]
                 [seesaw "1.4.5"]
                 ]
  :main ^:skip-aot toolbox.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
