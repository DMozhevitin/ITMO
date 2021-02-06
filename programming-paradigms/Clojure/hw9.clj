(definterface JExpr
  (JEvaluate [vars])
  (JDiff [diffvar])
  (JtoString []))

(declare ZERO)

(deftype JConst [value]
  JExpr
  (JEvaluate [_ _] value)
  (JDiff [_ _] ZERO)
  (JtoString [this] (format "%.1f" (double (.value this)))))

(def ZERO (JConst. 0))
(def ONE (JConst. 1))
(def TWO (JConst. 2))

(deftype JVariable [name]
  JExpr
  (JEvaluate [_ vars] (vars name))
  (JDiff [_ diffvar] (if (= name diffvar) ONE ZERO))
  (JtoString [_] (str name)))

(deftype JAbstractUnaryOperation [f diff c a]
  JExpr
  (JEvaluate [_ vars] (f (.JEvaluate a vars)))
  (JtoString [_] (str "(" c " " (.JtoString a) ")"))
  (JDiff [_ diffvar] (diff diffvar)))

(deftype JAbstractOperation [f diff c args]
  JExpr
  (JEvaluate [this vars] (apply f (map #(.JEvaluate % vars) (.args this))))
  (JtoString [this] (str "(" c " " (clojure.string/join " " (map #(.JtoString %) (.args this))) ")"))
  (JDiff [_ diffvar] (diff diffvar)))

(defn evaluate [obj vars] (.JEvaluate obj vars))
(defn toString [obj] (.JtoString obj))
(defn diff [expr diffvar] (.JDiff expr diffvar))
(defn Constant [value] (JConst. value))
(defn Variable [name] (JVariable. name))

(declare Cosh)
(declare Square)

(defn Add [& args] (JAbstractOperation. + (fn [diffvar] (apply Add (map #(diff % diffvar) args))) '+ args))
(defn Subtract [& args] (JAbstractOperation. - (fn [diffvar] (apply Subtract (map #(diff % diffvar) args))) '- args))
(defn Multiply [& args] (JAbstractOperation. *
                                             (fn [diffvar] (reduce #(Add (Multiply %1 (diff %2 diffvar)) (Multiply %2 (diff %1 diffvar))) args)) '* args))
(defn Divide [& args] (JAbstractOperation. #(/ (double %1) (double %2))
                                           (fn [diffvar] (reduce #(Divide (Subtract (Multiply %2 (diff %1 diffvar)) (Multiply %1 (diff %2 diffvar))) (Square %2)) args)) '/ args))


(defn Negate [a] (JAbstractUnaryOperation. - #(Negate (diff a %)) 'negate a))
(defn Sinh [a] (JAbstractUnaryOperation. #(Math/sinh %) #(Multiply (Cosh a) (diff a %)) 'sinh a))
(defn Cosh [a] (JAbstractUnaryOperation. #(Math/cosh %) #(Multiply (Sinh a) (diff a %)) 'cosh a))
(defn Sqrt [a] (JAbstractUnaryOperation. #(Math/sqrt (Math/abs %)) #(Multiply (Multiply (diff a %) a) (Divide ONE (Multiply TWO (Sqrt (Multiply (Square a) a))))) 'sqrt a))
(defn Square [a] (JAbstractUnaryOperation. #(* % %) #(Multiply (diff a %) (Multiply TWO a)) 'square a))

(def operations {'+ Add '- Subtract '* Multiply ' / Divide 'negate Negate 'sinh Sinh 'cosh Cosh 'square Square 'sqrt Sqrt})

(defn parse [expr]
  (cond
    (list? expr) (apply (operations (first expr)) (map parse (rest expr)))
    (number? expr) (Constant expr)
    :else (Variable (str expr))))

(defn parseObject [expression] (parse (read-string expression)))
