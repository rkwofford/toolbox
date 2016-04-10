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



  (def daily-from-epoch
    (let [start-date (java.util.GregorianCalendar. 1970 0 0 0 0)]
      (repeatedly
        (fn []
          (.add start-date java.util.Calendar/DAY_OF_YEAR 1)
          (.clone start-date)))))
(take 2 (drop 57 daily-from-epoch))

