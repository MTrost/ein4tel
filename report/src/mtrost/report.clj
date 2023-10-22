(ns mtrost.report
  {:nextjournal.clerk/toc true
   :nextjournal.clerk/visibility {:code :hide :result :hide}}
  (:require [clojure.string :as str]
            [nextjournal.clerk :as clerk]
            [tablecloth.api :as tc]
            [aerial.hanami.templates :as ht]
            [aerial.hanami.common :as hc :refer [RMV]]))

(def data (-> "src/mtrost/survey_2023.csv"
              tc/dataset))

(def start-string->description-str
  {"Um im meinem Alltag" {:label-prefix "Nutzung von "
                          :col-prefix "nutzung"}
   "... anderes Kommunikationsmittel, wenn" {:label-prefix "Sonst genutzt: "}
   "Wenn ich eine Frage" {:label-prefix "Finde Informationen über "}
   "... andere Informationsquelle, wenn"{:label-prefix ""}
   "Wie zufrieden bist du" {:label-prefix "Wie zufrieden mit"}
   "Gerne wäre ich über"{:label-prefix ""}})

^{:nextjournal.clerk/visibility {:result :show}}
(-> (filter
     (fn [k]
       (str/starts-with? k "Um im meinem Alltag"))
     (keys data)))

(defn- trunc [s]
  (->> (str/split s #" ")
       (take 4)
       (str/join " ")))

(defn- extract-tool [s]
  (second (re-find #"\[(.*)\]" s)))

(defn- rename-cols [col]
  (or (some->
       (:col-prefix (start-string->description-str (trunc col)))
       (str "_" (extract-tool col)))
      col))

(defn qst-kind [start qst]
  (str/starts-with? qst start))

(defn tool-kind [tool qst]
  (str/ends-with? qst (str tool "]")))


;; # EinViertel Umfrage
;; Im Mai und Juni ...
;; ## Auswertung
;; ### Nutzungs-Häufigkeit
;; Mit diesen Fragen wollten wir herausfinden, auf welchen offiziellen und inoffiziellen Kanälen
;; EinViertler:innen miteinander kommunizieren und wie oft.

;; Dazu stellten wir folgende Fragen:
;; Um im meinem Alltag mit meinen Nachbar:innen zu kommunizieren oder mich über Vorgänge im EinViertel zu informieren, benutze ich... (bitte das Zutreffende ankreuzen)"
;;   - Brauche ich nie
;;   - Brauche ich wenig
;;   - Brauche ich oft
;;   - Brauche ich täglich

(def qst "Um im meinem Alltag mit meinen Nachbar:innen zu kommunizieren oder mich über Vorgänge im EinViertel zu informieren, benutze ich... (bitte das Zutreffende ankreuzen)")

^{:nextjournal.clerk/visibility {:result :show}}
(def usage-data (tc/rename-columns
                 (tc/select-columns data (partial qst-kind "Um im meinem Alltag"))
                 extract-tool))

(re-find #"\b\pL*$" "Brauche ich täglich")

(def order {"nie" 0
            "wenig" 1
            "oft" 2
            "täglich" 3})

;; #### beUnity
^{:nextjournal.clerk/visibility {:result :show}}
(def usage-beunity
  (tc/aggregate
   (tc/drop-missing usage-data)
   {:brauche_ich #(reduce
                   (fn [acu cur]
                     (update acu
                             (let [fr (re-find #"\b\pL*$" cur)]
                               (str (order fr) "-" fr))
                             (fnil inc 0)))
                   {}
                   (% "beUnity"))}))


(clerk/vl (hc/xform ht/bar-chart
                    :DATA (map (fn [k]
                                 {:y (first (usage-beunity k))
                                  :x (-> k name (str/split #"-") last order)})
                               (reverse (keys usage-beunity)))
                          ;; => ({:y 38, :x 2} {:y 22, :x 3} {:y 8, :x 1} {:y 1, :x 0})

                    :X :x
                    :Y :y))

^{:nextjournal.clerk/visibility {:result :show}}
(clerk/vl {:data {:values (sort-by :order
                                   (map (fn [k]
                                          (let [label (-> k name (str/split #"-") last)]
                                            {:x (first (usage-beunity k))
                                             :y label
                                             :order (order label)}))
                                        (reverse (keys usage-beunity))))}
           :width 400
           :height 200
           :title "beUnity"
           :mark "bar"
           :encoding {:x {:field :x :type "quantitative"}
                      :y {:field :y :sort nil :title "Brauche ich ..."}}})


;; #### N-Cloud
;; #### Telefon
;; #### E-Mail
;; #### persönlicher Austausch

;; ### Informationen finden
;; ### Zufriedenheit

(clerk/col
 (->> (keys data)
      (map trunc)))

^{:nextjournal.clerk/visibility {:result :show}}
(clerk/col
 (->> (keys data)
      (map extract-tool)))



^{:nextjournal.clerk/visibility {:result :show}}
(clerk/vl (hc/xform ht/bar-chart
                    :DATA [{:a 10 :b 20} {:a 30 :b 40}]
                    :X :a
                    :Y :b))

(def used-tools
  [:beunity])

;; (let [[k v] (second data)]
;;  [k
;;   (map (fn [freq] {:freq freq}) v)])

^{:nextjournal.clerk/visibility {:result :show}}
(keys data)

(comment
  (clerk/show! *ns*))