(defn abstractOperation [g f & xs]
  (fn [vars]
    (g f (map #(% vars) xs))))

(defn abstractUnaryOperation [f x]
  (fn [vars]
    (f (x vars))))

(defn variable [name] (fn [vars] (vars name)))
(defn constant [arg] (constantly arg))

(def divide (partial abstractOperation reduce #(/ %1 (double %2))))
(def multiply (partial abstractOperation apply *))
(def subtract (partial abstractOperation apply -))
(def add (partial abstractOperation apply +))
(def negate (partial abstractUnaryOperation -))
(def square (partial abstractUnaryOperation #(* % %)))
(def sqrt (partial abstractUnaryOperation #(Math/sqrt (Math/abs %))))
(def min (partial abstractOperation reduce #(Math/min %1 %2)))
(def max (partial abstractOperation reduce #(Math/max %1 %2)))
(defn avg [& xs] (fn [vars] (/ ((apply add xs) vars) (count xs))))
(defn med [& xs] (fn [vars] (nth (sort (map #(% vars) xs)) (/ (count xs) 2))))


(def variables
  {'x (variable "x")
   'y (variable "y")
   'z (variable "z")})

(def operations
  {'+      add
   'negate negate
   '-      subtract
   '*      multiply
   '/      divide
   'med    med
   'avg    avg
   'min    min
   'max    max
   'square square
   'sqrt   sqrt})

(defn parse [expr]
  (cond
    (list? expr)
    (apply (operations (first expr))
           (map parse (rest expr)))
    (number? expr) (constant expr)
    (contains? variables expr) (variables expr)
    (contains? operations expr) (operations expr)))

(defn parseFunction [expression]
  (parse (read-string expression)))
