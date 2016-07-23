(ns sheep.core
  (:require [clojure.string :refer [split]])
  (:import [javax.sound.midi MidiSystem]))

(def thread-label #(str "[" (.getName (Thread/currentThread)) "]"))

(defn async-play-notes
  [synth notes ms]
  (future
    (let [label (thread-label)
          channel (aget (.getChannels synth) 0)]
      (println label "Playing" notes)
      (doseq [note notes] (.noteOn channel note 127))
      (Thread/sleep ms)
      (doseq [note notes] (.noteOff channel note))
      (println label "Goodbye!"))))

(defn play-notes
  [synth notes ms]
  (let [channel (aget (.getChannels synth) 0)]
    (doseq [note notes] (.noteOn channel note 127))
    (Thread/sleep ms)
    (.allNotesOff channel)))

(defn play-note
  [synth note ms]
  (let [channel (aget (.getChannels synth) 0)]
    (.noteOn channel note 127)
    (Thread/sleep ms)
    (.noteOff channel note 127)))

(defn play-sequence
  [synth sequence ms]
  (doseq [notes sequence]
    (play-notes synth notes ms)))

(defn play-melody
  [synth sequence ms]
  (doseq [note sequence]
    (play-notes synth [note] ms)))

;; (def synth (doto (MidiSystem/getSynthesizer) .open))

(defn synth [] (doto (MidiSystem/getSynthesizer) .open))

(def major-scale [2 4 5 7 9 11 12])
(def note-labels ["C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"])
(def intervals {0 :unison
                1 :minor-second
                2 :major-second
                3 :minor-third
                4 :major-third
                5 :perfect-fourth
                6 :tritone
                7 :perfect-fifth
                8 :minor-sixth
                9 :major-sixth
                10 :minor-seventh
                11 :major-seventh
                12 :octave})
(def degrees {1 0
              2 2
              3 4
              4 5
              5 7
              6 9
              7 11})

(def coin-flip #(= (rand-int 2) 1))

(defn rand-up
  [root]
  (with-open [s (synth)]
    (play-melody s [root (+ root (rand-nth major-scale))] 1800)))

(defn note-label
  [number]
  {:pre [(and (> number -1) (< number 128))]}
  (str (get note-labels (mod number 12)) (- (quot number 12) 1)))

(defn note-number
  [label]
  (let [[label octave] (case (count label)
                         2 [(subs label 0 1) (subs label 1)]
                         3 [(subs label 0 2) (subs label 2)])]
    (+ (* 12 (Integer/parseInt octave)) (.indexOf note-labels label))))

(defn play-interval
  []
  (let [distance (rand-nth (range 0 12))
        ascending (coin-flip)
        label (get intervals distance)
        start (rand-nth (range 36 83))
        end ((if ascending + -) start distance)
        interval [start end]
        answer (if (= label :unison)
                 label
                 (str label " " (if ascending :ascending :descending)))]
    (with-open [s (synth)]
      (play-melody s interval 1800)
      (Thread/sleep 1000)
      (println answer (map note-label interval) interval))))

(def one-through-five (take 5 degrees))

(defn rand-root []
  (let [key (rand-nth note-labels)
        octave 5]
    (note-number (str key octave))))

(defn foo []
  (with-open [s (synth)]
           (let [root (rand-root)
                 play #(play-note s % 3000)
                 melody (take 5 (repeatedly #(rand-nth (keys one-through-five))))]
             (println "Here's the root...")
             (play root)
             (doseq [note melody]
               (println "What degree is this?")
               (play (+ root (get degrees note)))
               (println "It's" note)))))

(defn bar []
  (with-open [s (synth)]
    (let [root (rand-root)
          play #(play-note s % 2000)
          melody (take 2 (repeatedly #(rand-nth (keys one-through-five))))]
      (doseq [note melody] (play (+ root (get degrees note))))
      (println melody))))
