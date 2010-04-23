(ns ical-test
  (:use ical :reload-all)
  (:use [clojure.test]))

(deftest run
  (is (not (mytest)))
  )
