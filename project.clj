(defproject asslangtool "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/tools.logging "1.3.0"]
                 [org.clojure/core.async "1.8.741"]]
  :main ^:skip-aot asslangtool.core 
  :profiles {:uberjar {:aot [asslangtool.core]
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}} 
  )
