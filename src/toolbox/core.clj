(ns toolbox.core
  (:gen-class))

(defn -main
  "Useful functions."
  [& args]
  (println "Useful functions"))

; A few simple statistical functions
(defn sum [x]  (reduce + x))

(defn square [x] (* x x))

(defn sum-of-squares [x] (reduce + (map square [x])))

(defn sample-mean [x] (/ (sum x) (count x)))

(defn sample-variance [x] ; using differnce of corrected sum of squares and square of sum
  (let [n (count x)
        ssx (sum-of-squares x)
        nm1 (- n 1)]
    (- (/ ssx nm1)
       (/ (square (sum x)) n nm1))
    ))

(defn afewstats
  "put some statistics in a vector"
  [x]
  ((juxt sum sum-of-squares sample-variance count) x)
  )

; Financial functions
(defn npv
  "Calculate Net Present Value given cash flows and discount rate"
  [rate cash-flows]
  (reduce + (map-indexed
              (fn [t cf] (/ cf (Math/pow (+ 1 rate) t)))
              cash-flows)))

(defn npv-derivative
  "Calculate derivative of NPV with respect to rate"
  [rate cash-flows]
  (reduce + (map-indexed
              (fn [t cf] (/ (* (- t) cf) (Math/pow (+ 1 rate) (+ t 1))))
              cash-flows)))

(defn irr
  "Calculate Internal Rate of Return for a series of cash flows using Newton-Raphson method.
   Cash flows should be a sequence where first value is typically negative (initial investment)
   and subsequent values are returns. Returns the rate where NPV = 0.

   Example: (irr [-1000 300 300 300 300]) => ~0.0779 (7.79%)"
  ([cash-flows] (irr cash-flows 0.1 0.0001 100))
  ([cash-flows initial-guess tolerance max-iterations]
   (loop [rate initial-guess
          iteration 0]
     (let [npv-val (npv rate cash-flows)
           npv-deriv (npv-derivative rate cash-flows)]
       (if (or (< (Math/abs npv-val) tolerance)
               (>= iteration max-iterations))
         rate
         (recur (- rate (/ npv-val npv-deriv))
                (inc iteration)))))))


  (def daily-from-epoch
    (let [start-date (java.util.GregorianCalendar. 1970 0 0 0 0)]
      (repeatedly
        (fn []
          (.add start-date java.util.Calendar/DAY_OF_YEAR 1)
          (.clone start-date)))))
(take 2 (drop 57 daily-from-epoch))

