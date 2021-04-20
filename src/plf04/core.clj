(ns plf04.core)

(defn string-e-1
  [s]
  (letfn [(g [c m]
            (cond
              (and (= c \e) (>= (m :c) 3)) {:r false :c (inc (m :c))}
              (and (= c \e) (>= (m :c) 0)) {:r true :c (inc (m :c))}
              :else m))
          (f [xs]
             (if (empty? xs)
               {:r false :c 0}
               (g (first xs) (f (rest xs)))))]
    ((f s) :r)))

(string-e-1 "Hello")
(string-e-1 "Hello")
(string-e-1 "Heelle")
(string-e-1 "Heelele")
(string-e-1 "hll")
(string-e-1 "e")
(string-e-1 "")

(defn string-e-2
  [s]
  (letfn [(g [x]
           (and (>= x 1) (<= x 3)))
          (h [x y]
             (if (= x \e) (inc y) y))
          (f[xs acc]
           (if (empty? xs)
           (g acc)
           (f (rest xs)
              (h (first xs) acc))))]
    (f s 0)))

(string-e-2 "Hello")
(string-e-2 "Heelle")
(string-e-2 "Heelele")
(string-e-2 "hll")
(string-e-2 "e")
(string-e-2 "")

(defn string-times-1
  [s n]
  (letfn [(f [x y]
            (cond (= y 0)
              ""
                  (> y 0)
              (str s (f x (dec y)))))]
    (f s n)))


(string-times-1 "Hi" 3)
(string-times-1 "Hi" 1)
(string-times-1 "Hi" 0)
(string-times-1 "Hi" 5)
(string-times-1 "Oh Boy!" 2)
(string-times-1 "x" 4)
(string-times-1 "code" 2)
(string-times-1 "code" 1)

(defn string-times-2
  [s n]
  (letfn [(f [x y w]
            (cond (= y 0)
              w
                  (> y 0)
              (f x (dec y) (str s w))))]
    (f s n "")))

(string-times-2 "Hi" 2)
(string-times-2 "Hi" 3)
(string-times-2 "Hi" 1)
(string-times-2 "Hi" 0)
(string-times-2 "Hi" 5)
(string-times-2 "Oh Boy!" 2)
(string-times-2 "x" 4)
(string-times-2 "code" 2)
(string-times-2 "code" 1)

(defn front-times-1
  [s n]
  (letfn [(f [x y]
            (cond (< (count x) 3)
                  (if (zero? y)
                    ""
                    (str s (f x (dec y))))
                  (zero? y)
                  ""
                  :else (str (first x) (first (rest x)) (first (rest(rest x))) (f x (dec y)))))]
    (f s n)))

(front-times-1 "Chocolate" 2)
(front-times-1 "Chocolate" 3)
(front-times-1 "Abc" 3)
(front-times-1 "Ab" 4)
(front-times-1 "A" 4)
(front-times-1 "" 4)
(front-times-1 "Abc" 0)

(defn front-Times-2
  [cadena n]
  (letfn [(f [x y w]
            (cond (< (count x) 3)
              (if (zero? y)
                w
                (f x (dec y) (str x w)))
              (zero? y)
                w
                :else (f x (dec y) (str (first x) (first (rest x)) (first (rest (rest x))) w))))]
    (f cadena n "")))

(front-Times-2 "Chocolate" 2)
(front-Times-2 "Chocolate" 3)
(front-Times-2 "Abc" 3)
(front-Times-2 "Ab" 4)
(front-Times-2 "A" 4)
(front-Times-2 "" 4)
(front-Times-2 "Abc" 0)

(defn count-xx-1
  [s]
  (letfn [(f [x]
            (cond (empty? x)
              0
              :else (+ (if (and (= \x (first x)) (= \x (first (rest x))))
                         1
                         0)
                 (f (rest x)))))]
    (f s)))

(count-xx-1 "abcxx")
(count-xx-1 "xxx")
(count-xx-1 "xxxx")
(count-xx-1 "abc")
(count-xx-1 "Hello there")
(count-xx-1 "Hexxo thxxe")
(count-xx-1 "")
(count-xx-1 "Kittens")
(count-xx-1 "Kittensxxx")

(defn count-xx-2
  [s]
  (letfn [(f [x y]
            (cond (empty? x)
              y
              :else (if (and (= \x (first x)) (= \x (first (rest x))))
                (f (rest x) (inc y))
                (f (rest x) y))))]
    (f s 0)))

(count-xx-2 "abcxx")
(count-xx-2 "xxx")
(count-xx-2 "xxxx")
(count-xx-2 "abc")
(count-xx-2 "Hello there")
(count-xx-2 "Hexxo thxxe")
(count-xx-2 "")
(count-xx-2 "Kittens")
(count-xx-2 "Kittensxxx")

(defn string-splosion-1
  [s]
  (letfn [(f [x]
            (cond (zero? (count x))
              x
              :else (str (f (subs x 0 (- (count x) 1))) x)))]
    (f s)))

(string-splosion-1 "Code")
(string-splosion-1 "abc")
(string-splosion-1 "ab")
(string-splosion-1 "x")
(string-splosion-1 "fade")
(string-splosion-1 "There")
(string-splosion-1 "Kitten")
(string-splosion-1 "Bye")
(string-splosion-1 "Good")
(string-splosion-1 "Bad")

(defn string-splosion-2
  [s]
  (letfn [(f [x y]
            (cond (== 0 (count x))
              y
              :else (f (subs x 0 (- (count x) 1)) (str x y))))]
    (f s "")))


(string-splosion-2 "Code")
(string-splosion-2 "abc")
(string-splosion-2 "ab")
(string-splosion-2 "x")
(string-splosion-2 "fade")
(string-splosion-2 "There")
(string-splosion-2 "Kitten")
(string-splosion-2 "Bye")
(string-splosion-2 "Good")
(string-splosion-2 "Bad")

(defn array-123-1
  [xs]
  (letfn [(f [ys]
            (cond (and (= 1 (first ys))
                     (= 2 (first (rest ys)))
                     (= 3 (first (rest (rest ys)))))
              true 
              (empty? ys) 
                  false
                  :else (f (rest ys))))]
    (f xs)))

(array-123-1 [1 1 2 3 1])
(array-123-1 [1 1 2 4 1])
(array-123-1 [1 1 2 1 2 3])
(array-123-1 [1 1 2 1 2 1])
(array-123-1 [1 2 3 1 2 3])
(array-123-1 [1 2 3])
(array-123-1 [1 1 1])
(array-123-1 [1 2])
(array-123-1 [1])
(array-123-1 [])


(defn array-123-2
  [xs]
  (letfn [(f [ys y]
            (cond (>= (count ys) 3)
                  (if (>= (count ys) 3)
                    (if (and (== (first ys) 1) (== (first (rest ys)) 2) (== (first (rest (rest ys))) 3))
                      (str y true)
                      (f (rest ys) y))
                    (f (empty ys) y))
                  (empty? y)
                  false
                  ))]
    (f xs "")))

(array-123-2 [1 1 2 4 1])
(array-123-2 [1 1 2 3 1])
(array-123-2 [1 1 2 1 2 3])
(array-123-2 [1 1 2 1 2 1])
(array-123-2 [1 2 3 1 2 3])
(array-123-2 [1 2 3])
(array-123-2 [1 1 1])
(array-123-2 [1 2])
(array-123-2 [1])
(array-123-2 [])

(defn string-x-1
  [s]
  (letfn [(f [x y]
            (cond (empty? x)
              ""
              (= y 0)
                (str (first x) (f (rest x) (inc y)))
                :else (if (and (= \x (first x)) (> (count x) 1))
                  (f (rest x) (inc y))
                  (str (first x) (f (rest x) (inc y))))))]
    (f s 0)))

(string-x-1 "xxHxix")
(string-x-1 "abxxxcd")
(string-x-1 "xabxxxcdx")
(string-x-1 "xKittenx")
(string-x-1 "Hello")
(string-x-1 "xx")
(string-x-1 "x")
(string-x-1 "")

(defn string-x-2
  [s]
  (letfn [(f [x y w]
            (cond (empty? x)
              w
              (= y 0)
                (str (first x) (f (rest x) (inc y) (str w)))
                :else (if (and (= \x (first x)) (> (count x) 1))
                  (f (rest x) (inc y) (str w))
                  (str (first x) (f (rest x) (inc y) w)))))]
    (f s 0 "")))

(string-x-2 "xxHxix")
(string-x-2 "abxxxcd")
(string-x-2 "xabxxxcdx")
(string-x-2 "xKittenx")
(string-x-2 "Hello")
(string-x-2 "xx")
(string-x-2 "x")
(string-x-2 "")

(defn altPairs-1
  [s]
  (letfn [(f [x y]
            (cond (== (count x) y)
              ""
              (or (== y 0) (== y 1) (== y 4) (== y 5) (== y 8) (== y 9))
                (str (str (first (drop y x))) (f s (inc y)))
                :else (f s (inc y))))]
    (f s 0)))

(altPairs-1 "kitten")
(altPairs-1 "Chocolate")
(altPairs-1 "CodingHorror")
(altPairs-1 "yak")
(altPairs-1 "ya")
(altPairs-1 "")
(altPairs-1 "ThisThatTheOther")

(defn altPairs-2
  [s]
  (letfn [(f [x y w]
            (cond (== (count x) y)
              w
              (or (== y 0) (== y 1) (== y 4) (== y 5) (== y 8) (== y 9))
                (f x (inc y) (str w (str(first (drop y x)))))
                :else (f x (inc y) w)))]
    (f s 0 "")))

(altPairs-2 "kitten")
(altPairs-2 "Chocolate")
(altPairs-2 "CodingHorror")
(altPairs-2 "yak")
(altPairs-2 "ya")
(altPairs-2 "")
(altPairs-2 "ThisThatTheOTher")

(defn stringYak-1
  [s]
  (letfn [(f [x z]
            (cond (empty? x)
              ""
              (and (= (first x) \y) (= (first (rest x)) \a) (= (first (rest (rest x))) \k) (> (count x) 1))
                (f (rest (rest (rest x))) (inc z))
                :else (str (first x) (f (rest x) (inc z)))))]
    (f s 0)))

(stringYak-1 "yakpak")
(stringYak-1 "pakyak")
(stringYak-1 "yak123ya")
(stringYak-1 "yak")
(stringYak-1 "yakxxxyak")
(stringYak-1 "HiyakHi")
(stringYak-1 "xxxyakyyyakzzz")

(defn stringYak-2
  [s]
  (letfn [(f [x y w]
            (cond (empty? x)
              w
              (and (= (first x) \y) (= (first (rest x)) \a) (= (first (rest (rest x))) \k) (> (count x) 1))
                (f (rest (rest (rest x))) (inc y) w)
                :else (f (rest x) (inc y) (str w (first x)))))]
    (f s 0 "")))

(stringYak-2 "yakpak")
(stringYak-2 "pakyak")
(stringYak-2 "yak123ya")
(stringYak-2 "yak")
(stringYak-2 "yakxxxyak")
(stringYak-2 "HiyakHi")
(stringYak-2 "xxxyakyyyakzzz")

(defn has-271-1
  [xs]
  (letfn [(f [ys]
            (cond (or (<= (count ys) 2) (empty? ys))
              false
              (and
                   (== (first (rest ys)) (+ (first ys) 5))
                   (<= (if (pos? (- (first (rest (rest ys))) (dec (first ys))))
                         (- (first (rest (rest ys))) (dec (first ys)))
                         (* -1 (- (first (rest (rest ys))) (dec (first ys))))) 2))
                true
                :else (f (rest ys))))]
    (f xs)))

(has-271-1 [1 2 7 1])
(has-271-1 [1 2 8 1])
(has-271-1 [2 7 1])
(has-271-1 [3 8 2])
(has-271-1 [2 7 3])
(has-271-1 [2 7 4])
(has-271-1 [2 7 -1])
(has-271-1 [2 7 -2])
(has-271-1 [4 5 3 8 0])
(has-271-1 [2 7 5 10 4])
(has-271-1 [2 7 -2 4 9 3])
(has-271-1 [2 7 5 10 1])
(has-271-1 [2 7 -2 4 10 2])
(has-271-1 [1 1 4 9 0])
(has-271-1 [1 1 4 9 4 9 2])

(defn has-271-2
  [xs acc]
  (letfn [(f [ys y]
            (cond (>= (count ys) 3)
                  (if (and (== (- (first (rest ys)) (first ys)) 5)
                           (>= 2 (if (neg? (- (first (rest (rest ys))) (- (first ys) 1)))
                                   (* -1 (- (first (rest (rest ys))) (- (first ys) 1)))
                                   (* 1 (- (first (rest (rest ys))) (- (first ys) 1))))))
                    true
                    (f (rest ys) y))
                  (<= (count ys) 3)
                  false
                  ))]
    (f xs acc)))

(has-271-2 [1 2 8 1] "")
(has-271-2 [1 2 7 1] "")
(has-271-2 [2 7 1] "")
(has-271-2 [3 8 2] "")
(has-271-2 [2 7 3] "")
(has-271-2 [2 7 4] "")
(has-271-2 [2 7 -1] "")
(has-271-2 [4 5 3 8 0] "")
(has-271-2 [2 7 -2] "")
(has-271-2 [2 7 5 10 4] "")
(has-271-2 [2 7 -2 4 9 3] "")
(has-271-2 [2 7 5 10 1] "")
(has-271-2 [2 7 -2 4 10 2] "")
(has-271-2 [1 1 4 9 0] "")
(has-271-2 [1 1 4 9 4 9 2] "")