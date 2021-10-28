(ns eva.core
  (:require [hashp.core]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen])
  (:gen-class))

(def weighing-factors (range 10 1 -1))

(defn char->int [c]
  (let [ascii-zero 48]
    (- (int c) 48)))

(defn calc-check-digit
  "Calculates the check digit of the first 9 digits of a numeric string"
  [number]
  (let [step-1 (map
                 (fn [a b]
                   (* (char->int a) b))
                 (take 9 number) weighing-factors)
        step-2 (reduce + step-1)
        step-3 (mod step-2 11)]
    (let [check-digit (- 11 step-3)]
      (case check-digit
        11 0
        10 -1
        check-digit))))

(defn modulus-11?
  "Checks whether numeric string abides the modulus-11 check.

  Examples:
    5990128088 => true
    1275988113 => true
    4536026665 => true


    5990128087 => false
    4536016660 => false"
  [number]
  (let [step-4 (calc-check-digit number)
        step-5 (let [check-digit (char->int (get number 9))]
                 (= check-digit step-4))]
    step-5))

(s/def ::digit (set (range 0 10)))
(s/def ::nhs-number (s/and string?
                           #(re-matches #"\d{10}" %)
                           #(modulus-11? %)))



(defn nhs-number?
  "Checks whether numeric string is an NHS Number"
  [number]
  (s/valid? ::nhs-number number))

(def digit-generator (s/gen ::digit))

(defn gen-nhs-number
  []
  "Generates a random NhsNumber. First generates 9 digits
  and then calculates their check digit. In case check digit is -1 (invalid)
  process is repeated"
  (let [gen-digits (fn [] (take 9 (repeatedly #(gen/generate digit-generator))))]
    (loop [digits (gen-digits)]
      (let [digits-str (apply str digits)
            check-digit (calc-check-digit digits-str)]
        (if (= -1 check-digit)
          (recur (gen-digits))
          (str digits-str check-digit))))))

(comment
  (def correct-examples ["5990128088" "1275988113" "4536026665"])

  (def wrong-examples ["5990128087" "4536016660"])

  (map nhs-number? correct-examples)

  (map nhs-number? wrong-examples)

  (nhs-number? (gen-nhs-number))
  )
