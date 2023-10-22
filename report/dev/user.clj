(ns user
  (:require [nextjournal.clerk :as clerk]))

(defn start!
  "Starts a Clerk server process "
  ([] (start! {}))
  ([opts]
   (let [defaults {:port 7777
                   :browse? true}]
     (clerk/serve!
      (merge defaults opts)))))

(comment
  (start! {:watch-paths ["src"]})
  )