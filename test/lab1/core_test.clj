(ns lab1.core-test
  (:require [clojure.test :refer :all]
            [lab1.core :refer :all]))

(deftest test-euclid-distance
  (is (=
    (euclid-distance [10 27] [5 15])
    13.0))

  (is (thrown? AssertionError (euclid-distance [5 2] [6 4 28]))))

(deftest test-calculate-potential
  (let [point1 {:coord [0.0 3.0], :potential 0.0}
        point2 {:coord [1.0 5.0], :potential 0.0}
        function-distance euclid-distance]

    (is (=
      (-> (calculate-potential point2 [point1 point2] function-distance) :potential)
      1.3701644213593966))))

(deftest test-hamming-distance
  (is (=
    (hamming-distance [2 1 7 3 8 9 6] [2 2 3 3 7 9 6])
    3))

  (is (thrown? AssertionError (hamming-distance [5 2] [6 4 28]))))

(deftest test-reset-max-point-potential
  (is (=
    (reset-max-point-potential
      [{:coord [5.0 8.0], :potential 3.0} {:coord [6.0 1.0], :potential 5.0}]
      {:coord [6.0 1.0], :potential 5.0})
    [{:coord [5.0 8.0], :potential 3.0} {:coord [6.0 1.0], :potential 0.0}])))

(deftest test-loading-points
  (let [path "test/points.txt"
        points [{:coord [5.0 8.0], :potential 0.0} {:coord [6.0 1.0], :potential 0.0}]]
    (is (=
      (loading-points path)
      points)))

  (is (thrown? AssertionError (loading-points nil)))

  (is (thrown?
    java.io.FileNotFoundException
    (loading-points "non-existing-file.txt"))))
