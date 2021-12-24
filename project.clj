(defproject introduction-to-clojure "0.1.0-SNAPSHOT"
  :description "Part of PurelyFunctional.tv"
  :url "https://purelyfunctional.tv/"
  :license {:name "CC0 1.0 Universal (CC0 1.0) Public Domain Dedication"
            :url "http://creativecommons.org/publicdomain/zero/1.0/"}
  :plugins [[org.clojure/core.unify "0.5.7"]
            [cider/cider-nrepl "0.24.0"]]
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [lispcast/bakery "1.0.0"]
                 [com.bhauman/rebel-readline "0.1.4"]]

  :main introduction-to-clojure.core)
