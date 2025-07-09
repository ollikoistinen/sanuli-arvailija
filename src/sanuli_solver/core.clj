(ns sanuli-solver.core
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.string :as str]
            [schema.core :as s]))

(s/defschema SanuliState
  {:correct-characters   [(s/pred #(s/maybe (char? %)))]
   :misplaced-characters [(s/pred #(s/maybe (char? %)))]
   :wrong-characters     [(s/pred #(s/maybe #{char? %}))]
   :accepted-characters  (s/pred #(s/maybe #{char? %}))
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

(defn word-valid-for-accepted-characters? [{:keys [accepted-characters]} word]
  (->> word
       (every? accepted-characters)))

(defn filter-words-by-state [state words]
  (let [valid-misplaced? (partial word-valid-for-misplaced-characters? state)
        valid-wrong?     (partial word-valid-for-wrong-characters? state)
        valid-correct?   (partial word-valid-for-correct-characters?
                                  (:correct-characters state))
        valid-accepted?  (partial word-valid-for-accepted-characters? state)]
    (->> words
         (filter valid-accepted?)
         (filter valid-misplaced?)
         (filter valid-wrong?)
         (filter valid-correct?))))

(defn word-score [state diversify? words word]
  (let [char-count       (:character-count state)
        word-char-freqs  (frequencies word)
        words-char-freqs (->> (range char-count)
                              (map (fn [i]
                                     (->> words
                                          (map #(nth % i)))))
                              (map frequencies))
        char-scores      (->> word
                              (map-indexed
                               (fn [idx word-char]
                                 (-> words-char-freqs
                                     (nth idx)
                                     (get word-char)))))]
    (if diversify?
      (->> word
           (map-indexed (fn [idx word-char]
                          (let [char-count-in-word (get word-char-freqs word-char)]
                            (-> (nth char-scores idx)
                                (/ char-count-in-word)))))
           (reduce + 0))
      (->> char-scores
           (reduce + 0)))))

(defn find-word [words state & {:keys [diversify?
                                       debug?]
                                :as   opts}]
  (let [valid-words  (filter-words-by-state state words)
        sorted-words (->> valid-words
                          (map (juxt identity
                                     #(word-score state
                                                  diversify?
                                                  valid-words
                                                  %)))

                          (sort-by second >))]
    (when debug?
      (clojure.pprint/pprint {:state             (dissoc state :accepted-characters)
                              :words-count       (count words)
                              :valid-words-count (count valid-words)
                              :diversify?        diversify?
                              :sorted-top-10     (take 10 sorted-words)}))
    (ffirst sorted-words)))

(defn read-words [{:keys [character-count]}]
  (with-open [reader (io/reader "resources/nykysuomensanalista2024.txt")]
    (let [csv-file-mem (memoize
                        #(doall
                          (csv/read-csv reader :separator \tab)))
          words-mem    (memoize
                        (fn []
                          (->> (csv-file-mem)
                               (drop 1)
                               (map first)
                               (filter #(= (count %) character-count)))))
          words        (words-mem)]
      words)))

(defn -main []
  (let [my-state (sanuli-state {:correct-characters   [nil nil nil nil nil]
                                :misplaced-characters [nil nil nil nil nil]
                                :wrong-characters     [#{} #{} #{} #{} #{}]
                                :accepted-characters  #{\a \b \c \d \e \f \g
                                                        \h \i \j \k \l \m \n
                                                        \o \p \q \r \s \t \u
                                                        \v \w \x \y \z \ä \ö
                                                        \å \A \B \C \D \E \F
                                                        \G \H \I \J \K \L \M
                                                        \N \O \P \Q \R \S \T
                                                        \U \V \W \X \Y \Z \Ä
                                                        \Ö \Å}
                                :character-count      5})
        words    (read-words my-state)]

    (find-word words my-state :debug? true :diversify? false)))

(-main)
