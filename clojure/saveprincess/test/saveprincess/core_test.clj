(ns saveprincess.core-test
  (:require [clojure.test :refer :all]
            [saveprincess.core :refer :all]))

(declare matrix-3x3 matrix2-3x3)

(deftest test-get-princess-coords
  (testing "Finds which corner the princess is in."
    (is (= [2 0] (get-princess-coords 3 matrix-3x3)))
    (is (= [0 2] (get-princess-coords 3 matrix2-3x3)))))

(deftest test-get-mario-coords
  (testing "Finds which corner mario is in."
    (is (= [0 1] (get-mario-coords 3 matrix-3x3)))
    (is (= [1 1] (get-mario-coords 3 matrix2-3x3)))))

(def matrix-3x3
  [
    '(\- \m \-)
    '(\- \- \-)
    '(\p \- \-)
  ])

(def matrix2-3x3
  [
    '(\- \- \p)
    '(\- \m \-)
    '(\- \- \-)
  ])
