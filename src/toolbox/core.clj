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
(defn pv
  "Calculate present value. Supports multiple arities:
   (pv rate) => discount factor for one period
   (pv rate t) => discount factor for t periods
   (pv rate t cashflow) => present value of cashflow at time t"
  ([] 0)
  ([rate] (/ 1 (+ 1 rate)))
  ([rate t] (Math/pow (pv rate) t))
  ([rate t cashflow] (* cashflow (pv rate t))))

(defn npv
  "Calculate Net Present Value given rate and cash flows.
   Two-arity version: assumes sequential time periods starting at t=0
   Three-arity version: accepts explicit time periods for each cash flow"
  ([rate cash-flows]
   (reduce + (map-indexed
               (fn [t cf] (pv rate t cf))
               cash-flows)))
  ([rate times cash-flows]
   (reduce + (map pv (repeat rate) times cash-flows))))

(defn npv-derivative
  "Calculate derivative of NPV with respect to rate"
  [rate cash-flows]
  (reduce + (map-indexed
              (fn [t cf] (/ (* (- t) cf) (Math/pow (+ 1 rate) (+ t 1))))
              cash-flows)))

(defn bounded-improve
  "Constrains improvement function to stay within bounds"
  [improve-fn lower upper]
  (fn [guess]
    (let [new-guess (improve-fn guess)]
      (cond
        (< new-guess lower) lower
        (> new-guess upper) upper
        :else new-guess))))

(defn irr
  "Calculate Internal Rate of Return for a series of cash flows using Newton-Raphson method.
   Cash flows should be a sequence where first value is typically negative (initial investment)
   and subsequent values are returns. Returns the rate where NPV = 0.

   Supports two forms:
   - (irr cash-flows) or (irr cash-flows guess) - assumes sequential time periods (0, 1, 2, ...)
   - (irr times cash-flows guess) - uses explicit time periods

   IRR is bounded between -99.99% and 1000% to ensure realistic values.

   Examples:
   (irr [-1000 300 300 300 300]) => ~0.0779 (7.79%)
   (irr [0 1 2 5] [-1000 300 300 960] 0.1) => IRR with non-sequential periods"
  ([cash-flows] (irr cash-flows 0.1))
  ([cash-flows-or-times guess-or-cashflows]
   (if (number? guess-or-cashflows)
     ; Two-arity: (irr cash-flows guess)
     (irr cash-flows-or-times guess-or-cashflows 0.0001 100)
     ; Two-arity: (irr times cash-flows) - use default guess
     (irr cash-flows-or-times guess-or-cashflows 0.1)))
  ([times-or-cashflows cashflows-or-guess tolerance-or-guess]
   (if (number? tolerance-or-guess)
     ; Three-arity case 1: (irr cash-flows guess tolerance)
     (irr times-or-cashflows cashflows-or-guess tolerance-or-guess 100)
     ; Three-arity case 2: (irr times cash-flows guess)
     (let [times times-or-cashflows
           cash-flows cashflows-or-guess
           guess tolerance-or-guess
           lower -0.9999  ; IRR cannot be less than -100%
           upper 10.0     ; IRR unlikely to exceed 1000%
           tolerance 0.0001
           max-iterations 100]
       (let [improve (fn [rate]
                       (let [npv-val (npv rate times cash-flows)
                             npv-deriv (reduce + (map (fn [t cf]
                                                        (/ (* (- t) cf)
                                                           (Math/pow (+ 1 rate) (+ t 1))))
                                                      times cash-flows))]
                         (- rate (/ npv-val npv-deriv))))
             bounded-improve-fn (bounded-improve improve lower upper)]
         (loop [rate guess
                iteration 0]
           (let [npv-val (npv rate times cash-flows)]
             (if (or (< (Math/abs npv-val) tolerance)
                     (>= iteration max-iterations))
               rate
               (recur (bounded-improve-fn rate)
                      (inc iteration)))))))))
  ([cash-flows initial-guess tolerance max-iterations]
   ; Four-arity: (irr cash-flows guess tolerance max-iterations)
   (let [lower -0.9999  ; IRR cannot be less than -100%
         upper 10.0]    ; IRR unlikely to exceed 1000%
     (let [improve (fn [rate]
                     (let [npv-val (npv rate cash-flows)
                           npv-deriv (npv-derivative rate cash-flows)]
                       (- rate (/ npv-val npv-deriv))))
           bounded-improve-fn (bounded-improve improve lower upper)]
       (loop [rate initial-guess
              iteration 0]
         (let [npv-val (npv rate cash-flows)]
           (if (or (< (Math/abs npv-val) tolerance)
                   (>= iteration max-iterations))
             rate
             (recur (bounded-improve-fn rate)
                    (inc iteration)))))))))


  (def daily-from-epoch
    (let [start-date (java.util.GregorianCalendar. 1970 0 0 0 0)]
      (repeatedly
        (fn []
          (.add start-date java.util.Calendar/DAY_OF_YEAR 1)
          (.clone start-date)))))
(take 2 (drop 57 daily-from-epoch))

