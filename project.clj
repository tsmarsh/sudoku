(defproject sudoko "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.logic "1.0.1"]]
  :main sudoku.sudoku
  :native-image {:name "sudoku"
                 :aot [sudoku.sudoku]
                 :opts ["--verbose"
                        "--report-unsupported-elements-at-runtime"
                        "--initialize-at-build-time"]}
  :profiles {:uberjar {:aot [sudoku.sudoku]}}
  )
