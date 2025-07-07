(ns sanuli-solver.core
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.set :as set]
            [schema.core :as s]))


;; TODO: Represent the state after a guess?
;; Guess instead of guesses here and add SanuliSolverState that contains SanuliStates?
(s/defschema SanuliState
  {:guesses              [s/Str]
   :correct-characters   [(s/pred #(s/maybe (char? %)))]
   :misplaced-characters [(s/pred #(s/maybe (char? %)))]
   :wrong-characters     [(s/pred #(s/maybe #{char? %}))]
   :character-count      s/Int})

(defn valid-sanuli-state!
  "Check if the given state is a valid Sanuli state."
  [state]
  (s/validate SanuliState state))

(defn sanuli-state [data]
  (->> data
       valid-sanuli-state!))

(defn word-valid-for-misplaced-characters? [{:keys [misplaced-characters
                                                    correct-characters
                                                    wrong-characters
                                                    character-count]}
                                            word]
  (let [mp-char-counts         (->> misplaced-characters
                                    (filter some?)
                                    frequencies
                                    set)
        mp-char-set            (set (keys mp-char-counts))
        mp-char-counts-in-word (->> word
                                    frequencies
                                    (filter #(mp-char-set (key %)))
                                    set)]
    (when (= mp-char-counts mp-char-counts-in-word)
      (->> mp-char-counts
           (map (fn [[mp-char mp-char-count]]
                  (let [valid-idxs-for-mp-char (->> (range character-count)
                                                    (keep
                                                     (fn [idx]
                                                       (when (and (not (some? (get correct-characters idx)))
                                                                  (not ((get wrong-characters idx) mp-char))
                                                                  (not= (get misplaced-characters idx) mp-char))
                                                         idx))))]
                    (->> valid-idxs-for-mp-char
                         (filter #(= (nth word %) mp-char))
                         count
                         (>= mp-char-count)))))
           (every? true?)))))

(defn word-valid-for-wrong-characters? [{:keys [correct-characters
                                                wrong-characters]}
                                        word]
  (let [all-wrong-chars (set (apply concat wrong-characters))]
    (->> word
         (map-indexed
          (fn [idx char]
            (or (some? (get correct-characters idx))
                (not (all-wrong-chars char)))))
         (every? true?))))

(defn word-valid-for-correct-characters? [correct-characters word]
  (->> correct-characters
       (map-indexed (fn [idx correct-char]
                      (if (some? correct-char)
                        (= correct-char (nth word idx))
                        true)))
       (every? true?)))

(defn filter-words-by-state [state words]
  (let [valid-misplaced? (partial word-valid-for-misplaced-characters? state)
        valid-wrong?     (partial word-valid-for-wrong-characters? state)
        valid-correct?   (partial word-valid-for-correct-characters?
                                  (:correct-characters state))]
    (->> words
         (filter valid-misplaced?)
         (filter valid-wrong?)
         (filter valid-correct?))))

(defn probability-score [state words word]
  (let [char-count (:character-count state)
        freqs      (->> (range char-count)
                        (map (fn [i]
                               (->> words
                                    (map #(nth % i)))))
                        (map frequencies))]
    (->> word
         (map-indexed
          (fn [idx word-char]
            (-> (nth freqs idx)
                (get word-char))))
         (reduce + 0))))

(defn find-word [words state]
  (let [valid-words  (filter-words-by-state state words)
        sorted-words (->> valid-words
                          (map (juxt identity #(probability-score state valid-words %)))

                          (sort-by second >))]
    (ffirst sorted-words)))

(defn read-words []
  (with-open [reader (io/reader "resources/nykysuomensanalista2024.txt")]
    (let [csv-file-mem (memoize
                        #(doall
                          (csv/read-csv reader :separator \tab)))
          words-mem    (memoize
                        (fn []
                          (->> (csv-file-mem)
                               (drop 1)
                               (map first)
                               (filter #(= (count %) 5)))))
          words        (words-mem)]
      words)))

(defn -main []
  (let [words     (read-words)
        freqs     (->> (range 5)
                       (map (fn [i]
                              (->> words
                                   (map #(nth % i)))))
                       (map frequencies))
        top-chars (->> freqs
                       (map #(into [] %))
                       (map #(sort-by second > %))
                       (map ffirst))
        my-state  (sanuli-state {:guesses              []
                                 :correct-characters   [nil nil nil nil nil]
                                 :misplaced-characters [nil nil nil nil nil]
                                 :wrong-characters     [#{} #{} #{} #{} #{}]
                                 :character-count      5})
        word      (find-word words my-state)]
    {:top-chars top-chars
     :word      word}))

;; TODO: Use as diverse as possible chars for guessing

(-main)
