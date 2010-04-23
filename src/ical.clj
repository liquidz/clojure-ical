(ns ical
  (:use simply)
  (:require [clojure.contrib.def :as cd])
  (:import [java.text SimpleDateFormat])
  (:require [clojure.contrib.str-utils2 :as su2])
  )

(declare uid timestamp event->string)

(defstruct ical-event :start :end :description :summary :klass :uid :timestamp)
(def *date-pattern* #"^([1-9][0-9]{3})[\/\-\.]?([01]?[0-9])[\/\-\.]?([0-3]?[0-9])$")

; =ical-header
(def ical-header
  (list "BEGIN:VCALENDAR" "VERSION:2.0"
        "METHOD:PUBLISH" "CALSCALE:GREGORIAN"
        "PRODID:iCalendar-Clojure"))

; =ical-footer
(def ical-footer
  (list "END:VCALENDAR"))

; =get-simple-date-format
(defn get-simple-date-format
  ([format date]
   (let [sdf (java.text.SimpleDateFormat. format)]
     (.format sdf date)
     )
   )
  ([format]
   (get-simple-date-format format (java.util.Date.))
   )
  )

; =date?
(defn date? [s]
  {:pre [(string? s)]}
  (not (nil? (re-matches *date-pattern* s)))
  )

; =n-digit
(defn n-digit
  ([s n c]
   {:pre [(string? s) (pos? n) (or (string? c) (char? c))]
    :post [(string? %)]}
   (let [st (if (string? s) s (str s))
         len (count st)]
     (if (< len n)
       (str (su2/join "" (take (- n len) (repeat (str c)))) st)
       st
       )
     )
   )
  ([s n] (n-digit s n "0"))
  )

; =make-ical
(defn make-ical [] (ref ()))

; =add-ical-event
(cd/defnk add-ical-event [cal :start "" :end "" :description "" :summary "" :klass "PRIVATE"]
  ;{:pre [(ref? cal) (!= start "") (!= end "") (!= description "")]}
  {:pre [(!= start "") (!= end "") (!= description "")]}
  (let [smry (if (= summary "") description summary)
        e (struct ical-event start end description smry klass (uid) (timestamp))
        ]
    (dosync
      (ref-set cal (cons e @cal))
      )
    )
  )

; =ical->string
(defn ical->string [cal]
  (su2/join 
    "\n"
    (concat
      ical-header
      (map #(event->string %) @cal)
      ical-footer
      )
    )
  )

; =event->string
(defn- event->string [e]
  {pre [(date? (:start e)) (date? (:end e))]}
  (let [[st sy sm sd] (re-matches *date-pattern* (:start e))
        [en ey em ed] (re-matches *date-pattern* (:end e))]
    (su2/join
      "\n"
      (list
        "BEGIN:VEVENT" "SEQUENCE:0"
        (str "DTEND:" ey (n-digit em 2) (n-digit ed 2))
        (str "DTSTART:" sy (n-digit sm 2) (n-digit sd 2))
        (str "UID:" (:uid e))
        (str "DTSTAMP:" (:timestamp e))
        (str "DESCRIPTION:" (:description e))
        (str "SUMMARY:" (:summary e))
        "END:VEVENT"
        )
      )
    )
  )

; =uid
(defn- uid []
  (let [r (reduce #(str %1 %2) (map inc (take 8 (iterate (fn [x] (rand-int 9)) 0)))) ]
    (su2/replace 
      (str (get-simple-date-format "yyyy-MM-dd HH:mm:ss") "+09:00_" r "@localhost")
      #"\s"
      "T"
      )
    )
  ) 

; =timestamp
(defn- timestamp []
  (su2/replace 
    (get-simple-date-format "yyyyMMDD HHmmss")
    #"\s"
    "T"
    )
  )

