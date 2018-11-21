(ns aoc.day-1
  (:require [clojure.string :as str])
  (:gen-class))


(defn flip [f]
  (fn [& xs]
    (apply f (reverse xs))))

(defn parse-int [str] (Integer/parseInt str))

(defn chars->int [c]
  (->> c
      (apply str)
      parse-int))

(defn dir->sym [dir]
  (case dir
    \L :left
    \R :right))

(defn filter-values [f coll]
  (reduce
   (fn [vals [k v]]
     (if (f k) (conj vals v) vals))
   []
   coll))

(defn abs [x] (Math/abs x))

(defn raw->input [raw]
  (map (fn [[dir & vals]]
        [(dir->sym dir) (chars->int vals)])
      raw))

(defn turn [current dir]
  (case [current dir]
    [:n :left] :w
    [:n :right] :e

    [:s :left] :e
    [:s :right] :w

    [:w :left] :s
    [:w :right] :n

    [:e :left] :n
    [:e :right] :s))

(defn move-y [{:keys [y] :as state} dir amount]
  (case dir
    :n (+ y amount)
    :s (- y amount)
    y))

(defn move-x [{:keys [x] :as state} dir amount]
  (case dir
    :e (+ x amount)
    :w (- x amount)
    x))

(defn move [state [to val]]
  (let [dir (turn (state :dir) to)
        x (move-x state dir val)
        y (move-y state dir val)]
    {:dir dir :x x :y y}))

(def raw "R1, R3, L2, L5, L2, L1, R3, L4, R2, L2, L4, R2, L1, R1, L2, R3, L1, L4, R2, L5, R3, R4, L1, R2, L1, R3, L4, R5, L4, L5, R5, L3, R2, L3, L3, R1, R3, L4, R2, R5, L4, R1, L1, L1, R5, L2, R1, L2, R188, L5, L3, R5, R1, L2, L4, R3, R5, L3, R3, R45, L4, R4, R72, R2, R3, L1, R1, L1, L1, R192, L1, L1, L1, L4, R1, L2, L5, L3, R5, L3, R3, L4, L3, R1, R4, L2, R2, R3, L5, R3, L1, R1, R4, L2, L3, R1, R3, L4, L3, L4, L2, L2, R1, R3, L5, L1, R4, R2, L4, L1, R3, R3, R1, L5, L2, R4, R4, R2, R1, R5, R5, L4, L1, R5, R3, R4, R5, R3, L1, L2, L4, R1, R4, R5, L2, L3, R4, L4, R2, L2, L4, L2, R5, R1, R4, R3, R5, L4, L4, L5, L5, R3, R4, L1, L3, R2, L2, R1, L3, L5, R5, R5, R3, L4, L2, R4, R5, R1, R4, L3")

(def split (partial (flip str/split) #", "))

(->>
  raw
  split
  (raw->input)
  (reduce move {:dir :n :x 0 :y 0})
  (filter-values #{:x :y})
  (map abs)
  (reduce +))

