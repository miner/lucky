(println "Loading the dev-only ./dev/user.clj for Lucky")

(load-file "/Users/miner/cljproj/user.clj")

(require '[miner.lucky :refer :all])
(println "using [miner.lucky]")

(require '[clojure.data.avl :as avl])
(println "(require '[clojure.data.avl :as avl])")

(flush)
