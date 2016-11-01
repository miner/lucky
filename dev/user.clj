(println "Loading the dev-only ./dev/user.clj for Lucky")

(load-file "/Users/miner/Dropbox/clj/miner.clj")

(require '[miner.lucky :refer :all])
(println "using [miner.lucky]")

(require '[clojure.data.avl :as avl])
(println "(require '[clojure.data.avl :as avl])")

(flush)
