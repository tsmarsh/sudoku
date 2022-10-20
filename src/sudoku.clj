(ns sudoku
  (:require [clojure.core.logic.fd :as fd]
            [clojure.core.logic :as cl]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn get-square [rows x y]
  (for [x (range x (+ x 3))
        y (range y (+ y 3))]
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
        rows (->> vars (partition 9) (map vec) (into []))
        cols (apply map vector rows)
        sqs  (for [x (range 0 9 3)
                   y (range 0 9 3)]
               (get-square rows x y))]
    (cl/run 1 [q]
      (cl/== q vars)
      (cl/everyg #(fd/in % (fd/domain 1 2 3 4 5 6 7 8 9)) vars)
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

(defn -main []
  (let [hints (parseHints (io/resource "test.txt"))
        _ (println hints)
        result (time (sudokufd hints))]
    (doall (map println (partition 9 (first hints))))
    (doall (map println (partition 9 (first result))))))
