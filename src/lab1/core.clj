(ns lab1.core
  (:gen-class))
  (def Emax 0.6)
(def Emin 0.2)
(def Ra 3.0)
(def Rb (* 1.5 Ra))

(def potential_b_koeff (/ 4 (Math/pow Rb 2)))
(def potential_a_koeff (/ 4 (Math/pow Ra 2)))

(declare max-pot-point)
(declare review-potentials)
(declare short-distance)


(defn euclid-distance [coord1 coord2]
  {:pre [(= (count coord1) (count coord2))]}
  (Math/sqrt (apply + (map #(Math/pow (- %1 %2) 2) coord1 coord2))))

(defn hamming-distance [coord1 coord2]
  {:pre [(= (count coord1) (count coord2))]}
  (apply + (map #(if (= %1 %2) 0 1) coord1 coord2)))

(defn reset-max-point-potential [points target-point]
  (map
    (fn [point]
      (if
        (=
          (point :coord)
          (target-point :coord))
        (assoc point :potential 0.0)
        point))
    points))

(defn find-centers
  ([points function-distance]
    (find-centers points nil [] function-distance))

  ([points first-center-potential centers function-distance]

    (let [max-point (max-pot-point points)
         reviewed-points (review-potentials points max-point function-distance)]

      (if-not first-center-potential
        (recur reviewed-points (max-point :potential) [max-point] function-distance)
        (if
          (>
            (max-point :potential)
            (* Emax first-center-potential))
          (recur reviewed-points first-center-potential (conj centers max-point) function-distance)
          (if
            (<
              (max-point :potential)
              (* Emin first-center-potential))
            centers
            (if
              (>=
                (+
                  (/ (short-distance max-point centers function-distance) Ra)
                  (/ (max-point :potential) first-center-potential))
                1)
              (recur reviewed-points first-center-potential (conj centers max-point) function-distance)
              (recur
                (reset-max-point-potential reviewed-points max-point)
                first-center-potential
                centers
                function-distance))))))))

(defn loading-points [path]
  {:pre [(not (nil? path))]}
  (with-open [r (clojure.java.io/reader path)]
    (doall (map (fn [line]
      {:coord (vec (map #(Double/parseDouble %) (drop-last (.split #"," line))))
        :potential 0.0})
      (line-seq r)))))


(defn calculate-potential [point, points, function-distance]
  (let [new-potential
        (reduce
          (fn [res other-point]
          (+
            res
            (Math/pow
              Math/E
              (*
                (- potential_a_koeff)
                (function-distance
                  (point :coord)
                  (other-point :coord))))))
            0.0
            points)]

    (assoc point :potential new-potential)))

(defn calculate-potentials [points function-distance]
  (map #(calculate-potential % points function-distance) points))

(defn review-potential [point points base-point function-distance]
  (let [revised-potential
        (-
          (point :potential)
          (*
            (base-point :potential)
            (Math/pow
              Math/E
              (*
                (- potential_b_koeff)
                (function-distance
                  (point :coord)
                  (base-point :coord))))))]

    (assoc point :potential revised-potential)))

(defn review-potentials [points base-point function-distance]
  (map #(review-potential % points base-point function-distance) points))

(defn max-pot-point [points]
  (apply max-key
    (fn [point] (point :potential))
    points))

(defn short-distance [point points function-distance]
  (Math/sqrt (apply min (map #(function-distance (point :coord) (% :coord)) points))))

(defn -main
  [distance-method file]
  (let [function-distance (case distance-method
            "euclid" euclid-distance
              "hamming" hamming-distance)
        points (loading-points file)
        points (calculate-potentials points function-distance)]
    (doseq [center (find-centers points function-distance)] (println center))))
