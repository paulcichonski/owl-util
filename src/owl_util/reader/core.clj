(ns ^{:author "Paul Cichonski"
      :doc "Defines the core function interfaces (e.g., multimethods) to be used by rest of the reader codebase.
      
      Goal is to allow different input syntax of the same modeling langueages (e.g., OWL) to work with same reader function.
      "
      }
     owl-util.reader.core
  (:require [clojure.string :as string]))


(comment "these syntax tests are very simplistic, probably need to improve")
(defn check-syntax [file-location]
  "Simple tests to check the file to determine the syntax of the OWL model.
   Current types are - ::XML, "
  (with-open [r (java.io.BufferedReader. (java.io.FileReader. file-location))]
              (let [line (.readLine r)]
                (cond 
                  (not (string/blank? (re-find #"\<\?xml+" line))) ::XML
                  ))))


(comment "Primary reader method to use to get a map representation of an ontology from some OWL file. 
Returns a two-key map (:namespaces & :model), where :namespaces contains a map of all ns declarations 
(key is the qname prefix, value is the namespace. note: default namespace has a blank ':' keyword, which 
may need to be generated by a call to (keyword "")).
:model contains a list of maps, where each map is a class and the predicates that 
have that class as their domain.")
(defmulti classes-and-relationships check-syntax)