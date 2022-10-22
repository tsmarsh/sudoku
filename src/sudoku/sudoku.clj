(ns sudoku.sudoku
  (:gen-class)
  (:require [clojure.core.logic.fd :as fd]
            [clojure.core.logic :as cl]
            [clojure.string :as str]))

(def square-size 3)

(defn get-square [rows x y]
  (for [x (range x (+ x square-size))
        y (range y (+ y square-size))]
    (get-in rows [x y])))

(defn init [vars hints]
  (if (seq vars)
    (let [hint (first hints)]
      (cl/all
        (if-not (zero? hint)
          (cl/== (first vars) hint)
          cl/succeed)
        (init (next vars) (next hints))))
    cl/succeed))

(defn sudokufd [hints]
  (let [vars (repeatedly 81 cl/lvar)
        rows (->> vars (partition (* square-size square-size)) (map vec) (into []))
        cols (apply map vector rows)
        sqs  (for [x (range 0 (* square-size square-size) square-size)
                   y (range 0 (* square-size square-size) square-size)]
               (get-square rows x y))]
    (cl/run 1 [q]
      (cl/== q vars)
      (cl/everyg #(fd/in % (apply fd/domain (range 1 (inc (* square-size square-size))))) vars)
      (init vars hints)
      (cl/everyg fd/distinct rows)
      (cl/everyg fd/distinct cols)
      (cl/everyg fd/distinct sqs))))

(defn parseHints [f]
  (let [strings (slurp f)
        lines (str/split-lines strings)
        chrs (map #(str/split % #" ") lines)
        flat_chars (flatten chrs)]
    (map #(Integer/parseInt %) flat_chars)))

(defn -main [& args]
  (let [hints (parseHints *in*)
        result (time (sudokufd hints))
        ]
    (println "Hints: \n")
    (doall (map println (partition 9 hints)))

    (println "Solutions:" (count result) "\n")
    (doall (map println (partition 9 (first result))))
    ))
