(ns sudoku.sudoku_test
  (:require [clojure.test :refer :all]
            [sudoku.sudoku :refer [sudokufd]]))

(def test [0 0 6 3 1 7 8 0 0
           3 0 0 0 0 0 0 0 1
           0 0 7 0 0 0 3 0 0
           7 0 0 9 0 6 0 0 8
           0 6 0 0 0 0 0 4 0
           8 0 0 5 0 4 0 0 7
           0 0 1 0 0 0 5 0 0
           4 0 0 0 0 0 0 0 6
           0 0 8 7 5 1 2 0 0])

(def hard [0 7 0 8 0 0 0 9 0
           9 0 0 0 7 0 0 0 3
           0 0 0 5 0 0 0 7 0
           1 0 0 0 8 4 0 0 0
           7 0 0 0 0 0 0 8 1
           8 0 0 0 5 0 4 0 0
           0 0 0 0 0 0 9 2 0
           4 9 0 0 0 3 1 0 8
           0 0 6 0 2 8 3 0 0])

(deftest solving-puzzles
  (testing "can solve a puzzle"
    (let [soln [9 4 6 3 1 7 8 2 5
                3 8 2 6 9 5 4 7 1
                1 5 7 8 4 2 3 6 9
                7 2 4 9 3 6 1 5 8
                5 6 3 1 7 8 9 4 2
                8 1 9 5 2 4 6 3 7
                2 7 1 4 6 9 5 8 3
                4 9 5 2 8 3 7 1 6
                6 3 8 7 5 1 2 9 4]]
      (is (= soln (first (sudokufd test)))))))


(deftest solving-hard-puzzles
  (testing "can solve a hard puzzle"
    (let [soln [2 7 5 8 3 1 6 9 4
                9 4 8 6 7 2 5 1 3
                6 3 1 5 4 9 8 7 2
                1 6 9 2 8 4 7 3 5
                7 5 4 3 9 6 2 8 1
                8 2 3 1 5 7 4 6 9
                3 8 7 4 1 5 9 2 6
                4 9 2 7 6 3 1 5 8
                5 1 6 9 2 8 3 4 7]]
      (is (= soln (first (sudokufd hard)))))))


