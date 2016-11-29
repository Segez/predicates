(ns predicates)

(defn sum-f [f g x]
  (+ (f x) (g x)))

(defn less-than [n]
  (fn [k] (< k n)))

(defn equal-to [n]
  "equal-to function use '==' operator to compare args as a number"
  (fn [e] (== e n)))

(defn set->predicate [a-set]
  (fn [ky] (contains? a-set ky)))

(defn pred-and [pred1 pred2]
  (fn [x] (and (pred1 x) (pred2 x))))

(defn pred-or [pred1 pred2]
  (fn [x] (or (pred1 x) (pred2 x))))

(defn whitespace? [character]
  (Character/isWhitespace character))

(defn blank? [string]
  (every? whitespace? string))

(defn has-award? [book award]
  ((set->predicate (:awards book)) award))

(defn HAS-ALL-THE-AWARDS? [book awards]
  (every? (set->predicate (:awards book)) awards))

(defn my-some [pred a-seq]
  (first (filter (fn [x] (pred x)) a-seq)))

(defn my-some [pred a-seq]
  (let [result (filter pred a-seq)]
    (if (empty? result)
      false
      (pred (first result)))))

(defn my-every? [pred a-seq]
  (if (=
        (count (filter pred a-seq))
        (count a-seq))
    true false))

(defn prime? [n]
  (let [pred (fn [nmbr] (== (mod n nmbr) 0))]
    (not (some pred (range 2 n)))))


