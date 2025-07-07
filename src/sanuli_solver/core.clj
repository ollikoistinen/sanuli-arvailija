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
                                                    character-count]}
                                            word]
  (->> misplaced-characters
       (map-indexed
        (fn [misplaced-idx misplaced-character]
          (if (some? misplaced-character)
            (let [misplaced-char-correct-idxs (->> correct-characters
                                                   (keep-indexed (fn [correct-character-idx correct-character]
                                                                  (when (and (some? correct-character)
                                                                             (= correct-character misplaced-character))
                                                                    correct-character-idx)))
                                                   set)
                  disallowed-idxs             (-> misplaced-char-correct-idxs
                                                  (conj misplaced-idx))
                  look-idxs                   (-> character-count
                                                  range
                                                  set
                                                  (set/difference disallowed-idxs))]
               ;; TODO: Not optimal when there are multiple same misplaced characters
               ;; Check for count instead of existence should work.
              (->> look-idxs
                   (map #(nth word %))
                   (some #(= % misplaced-character))))
            true)))
       (every? true?)))

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

(defn find-word [words state]
  (let [valid-misplaced? (partial word-valid-for-misplaced-characters? state)
        valid-wrong?     (partial word-valid-for-wrong-characters? state)
        valid-correct?   (partial word-valid-for-correct-characters?
                                  (:correct-characters state))]
    (->> words
         (filter valid-misplaced?)
         (filter valid-wrong?)
         (filter valid-correct?)
         first)))

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
                                 :wrong-characters     [nil nil nil nil nil]
                                 :character-count      5})
        word      (find-word words my-state)]
    {:top-chars top-chars
     :word      word}))

;; TODO: Use char frequencies for guessing

(-main)
