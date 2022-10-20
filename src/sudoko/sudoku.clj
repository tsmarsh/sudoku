(ns sudoku
  (:require [clojure.core.logic.fd :as fd]
            [clojure.core.logic :as cl]))

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

;; ====

(time (sudokufd
 [0 0 6 3 1 7 8 0 0
  3 0 0 0 0 0 0 0 1
  0 0 7 0 0 0 3 0 0
  7 0 0 9 0 6 0 0 8
  0 6 0 0 0 0 0 4 0
  8 0 0 5 0 4 0 0 7
  0 0 1 0 0 0 5 0 0
  4 0 0 0 0 0 0 0 6
  0 0 8 7 5 1 2 0 0]))
