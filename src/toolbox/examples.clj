(ns toolbox.examples)

; Examples of working with datasets from Incanter
(use 'incanter.core)
(to-dataset 1)
(to-dataset :a)
(to-dataset [:a])
(to-dataset (range 10))
(to-dataset (range 10) :transpose true)
(to-dataset [[1 2] [3 4] [5 6]])
(to-dataset [{:a 1 :b 2 :c 3}])
(to-dataset [{"a" 1 "b" 2 "c" 3}])
(to-dataset [{:a 1 :b 2} {:a 1 :b 2}])
(to-dataset [{"a" 1 "b" 2 "c" 3} {"a" 1 "b" 2 "c" 3}])

(rand-int 1000)

(println (map #(+ 8 %) (range 100)))

(println
  (let [x 5, y 6]
    (map
      (juxt
        (partial + 1)
        (partial * 8))
      [x y])
    )
  )
