
(ns aoc.day-1
  (:require [clojure.string :as str])
  (:gen-class))

(def input "R1, R3, L2, L5, L2, L1, R3, L4, R2, L2, L4, R2, L1, R1, L2, R3, L1, L4, R2, L5, R3, R4, L1, R2, L1, R3, L4, R5, L4, L5, R5, L3, R2, L3, L3, R1, R3, L4, R2, R5, L4, R1, L1, L1, R5, L2, R1, L2, R188, L5, L3, R5, R1, L2, L4, R3, R5, L3, R3, R45, L4, R4, R72, R2, R3, L1, R1, L1, L1, R192, L1, L1, L1, L4, R1, L2, L5, L3, R5, L3, R3, L4, L3, R1, R4, L2, R2, R3, L5, R3, L1, R1, R4, L2, L3, R1, R3, L4, L3, L4, L2, L2, R1, R3, L5, L1, R4, R2, L4, L1, R3, R3, R1, L5, L2, R4, R4, R2, R1, R5, R5, L4, L1, R5, R3, R4, R5, R3, L1, L2, L4, R1, R4, R5, L2, L3, R4, L4, R2, L2, L4, L2, R5, R1, R4, R3, R5, L4, L4, L5, L5, R3, R4, L1, L3, R2, L2, R1, L3, L5, R5, R5, R3, L4, L2, R4, R5, R1, R4, L3")


;; plumbing

(def initial-state {:dir :n
                    :x 0
                    :y 0
                    :visited #{{:x 0 :y 0}}})

(defn flip [f]
  (fn [& xs]
    (apply f (reverse xs))))

(defn parse-int [str] (Integer/parseInt str))

(defn chars->int [c]
  (->> c
      (apply str) parse-int))

(defn dir->sym [dir]
  (case dir
    \L :left
    \R :right))

;; gets all values in coll that the
;; the predicate f returns true for
(defn filter-values [f coll]
  (reduce
   (fn [vals [k v]]
     (if (f k) (conj vals v) vals))
   []
   coll))

(defn abs [x] (Math/abs x))

;; Translates L32 -> [:left 32]
(defn raw->pairs [raw]
  (map (fn [[dir & vals]]
        [(dir->sym dir) (chars->int vals)])
      raw))

(defn turn-to [from to]
  (case [from to]
    [:n :left] :w
    [:n :right] :e

    [:s :left] :e
    [:s :right] :w

    [:w :left] :s
    [:w :right] :n

    [:e :left] :n
    [:e :right] :s))

;; updates state with new direction
(defn turn [state to]
  (update state :dir turn-to to))

;; maps an direction to an axis and corresponding action to
;; take a single step
(def ops {:n [:y inc]
          :s [:y dec]
          :e [:x inc]
          :w [:x dec]})

;; returns a lazy sequence of steps walkning in the current
;; direction by applying an operator from ops to the current
;; state recursively
(defn get-axis [state]
  (let [[k f] (ops (state :dir))
        update (fn [state] (update state k f))
        iterator (iterate update state)]
    (drop 1 iterator)))

;; walks amount steps in the current direction
;; and returns the full history of the steps walked
(defn get-steps-walked [state steps]
  (let [axis (get-axis state)]
      (take steps axis)))

;; moves the state from to the a new position [:left|right amount]
;; returns the new state as well as all the steps walked
(defn walk-to [state [to steps]]
  (let [state (turn state to)
        walked (get-steps-walked state steps)
        state (last walked)
        visited (map #(select-keys %1 [:x :y]) walked)]
    [state visited]))

;; reducing function; walks until a cycle has been found.
;; returns the cyclic step.
(defn find-cycle [state next]
  (let [[state visited] (walk-to state next)
        candidates (filter (state :visited) visited)]
    (if-let [match (first candidates)]
      (reduced match)
      (update state :visited into visited))))

(def split (partial (flip str/split) #", "))

(->>
  input
  split
  (raw->pairs)
  (reduce find-cycle initial-state)
  (filter-values #{:x :y})
  (map abs)
  (reduce +))
