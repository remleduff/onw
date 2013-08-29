(ns onw.core
  (:require [incanter
             [core :as c]
             [stats :as s]
             [charts :as charts]]
            [clojure.math.combinatorics :as m]))

(defn all-possible [& {:keys [hand] :as options}]
  (assoc options :votes (range (count hand))))

(defn remove-self [& {:keys [votes] :as options}]
  )

(def behaviours {:all {:vote [all-possible remove-self]}})

  (map-indexed (fn [idx itm]
                 (let [valid (disj (set (range (count hand))) idx)]
                   (rand-nth (vec valid)))) hand))

(defn werewolf? [hand n]
  (= :werewolf (hand n)))

(defn nowolf-guess [hand]
  (map-indexed (fn [idx itm]
                 (let [valid (disj (set (range (count hand))) idx)
                       valid (if (werewolf? hand idx)
                               (remove (partial werewolf? hand) valid)
                               valid)]
                   (rand-nth (vec valid)))) hand))

(defn lynched? [most e] (and (> (val e) 1) (= most (val e))))

(defn vote [guess]
  (let [freqs (->> guess frequencies (sort-by val) reverse)
        most (-> freqs first val)]
    (->> freqs
         (take-while (partial lynched? most))
         (map key)
         set)))

(defn dead [hand guess]
  (let [lynches (vote guess)
        hunter (.indexOf hand :hunter)
        lynches (if (>= hunter 0)
                  (conj lynches (nth guess hunter))
                  lynches)]
    lynches))

(defn mayor-guess [hand old-deads]
  (map-indexed (fn [idx itm]
                 (let [valid (disj (set (range (count hand))) idx)
                       valid (if (werewolf? hand idx)
                               (remove (partial werewolf? hand) valid)
                               valid)]
                   (rand-nth (vec valid)))) hand))

(defn dead-cards [deck hand]
  (loop [result []
         deck (sort deck)
         hand (sort hand)]
    (if (< (count result) 2)
      (let [card (first deck)]
        (if (-> hand set card)
          (recur result (rest deck) (rest hand))
          (recur (conj result card) (rest deck) (rest hand))))
      result)))

(defn mayor-vote [deck hand deads]
  (let [new-mayor-card (s/sample (dead-cards deck hand) :size 1)
        guess (mayor-guess deck hand guess deads)]
    ))

(defn winner [deck hand guess]
  (let [deads (dead hand guess)
        lynches (map hand dead)]
    (cond
;;      (some #{:mayor} lynches)
;;      (mayor-vote deck hand deads)

     (some #{:martyr} lynches)
     :martyr

     (some #{:werewolf} lynches)
     :villager

     (and (nil? (some #{:werewolf} hand))
              (empty? lynches))
     :all

     :else
     :werewolf)))

(defn score [type winner]
  (if (or (= winner :all) (= type winner)) 1 0))

(defn chance [deck hands trials type guesses]
  (let [wins (c/sum (map (partial score type) (map (partial winner deck) hands guesses)))]
    (Math/round (* 100.0 (/ wins trials)))))

(defn possible-hands [deck player-count]
  (let [roles (map vec (m/combinations deck player-count))]
    roles)

(defn make-deck [roles player-count]
  (let [missing-cards (- (+ player-count 2) (count roles))]
    (concat roles (repeat missing-cards :villager))))

(defn compute-chance [roles player-count trials]
  (let [deck (make-deck roles player-count)
        possible-hands (possible-hands deck player-count)
        hands (s/sample possible-hands :size trials)
        random-guesses (take trials (repeatedly #(s/sample (range player-count))))
        noself-guesses (map noself-guess hands)
        nowolf-guesses (map nowolf-guess hands)
        village-chance (partial chance deck hands trials :villager)
        martyr-chance (partial chance deck hands trials :martyr)]
    (map (juxt village-chance martyr-chance) [random-guesses noself-guesses nowolf-guesses])))

(def n 1000)
(compute-chance [:werewolf :martyr] 2 n)
(compute-chance [:werewolf :seer :thief] 3 n)
(compute-chance [:werewolf :seer :thief :hunter] 3 n)
(compute-chance [:werewolf :seer :thief :martyr] 3 n)
(compute-chance [:werewolf :seer :thief :hunter :martyr] 3 n)
(def cards [:werewolf :werewolf :seer :thief :hunter :martyr])
(compute-chance cards 3 n)
(compute-chance cards 4 n)
(compute-chance cards 5 n)
(compute-chance cards 6 n)
(compute-chance cards 7 n)
(compute-chance cards 8 n)
(compute-chance cards 9 n)
(compute-chance cards 10 n)

(comment
  (noself-guess [:w :w :v :t])
  (nowolf-guess [:werewolf :villager :werewolf :villager])

  (vote [0 2 1 3])
  (dead [:villager :werewolf :werewolf :villager] [0 0 1 1 3 3])

  (winner [:werewolf :werewolf :villager :villager] [2 3])
  (winner [:werewolf :werewolf :villager :villager] [0 0 1 3])
  (winner [:werewolf :werewolf :villager :villager] [1 1 2 2])
  (winner [:seer :thief :villager :villager] [1 1 1 1])
  (winner [:seer :thief :villager :villager] [0 1 2 3])
)





