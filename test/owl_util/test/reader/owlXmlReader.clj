(ns owl-util.test.reader.owlXmlReader
  (:use [owl-util.reader.owlXmlReader]
        [clojure.test])
  (:require [owl-util.reader.core :as reader-core])) 

(def file-loc "test/owl_util/test/data/test-usecase.owl")

(deftest test-find-namespaces []
  (let [namespaces (find-namespaces (create-xml-ontology file-loc))]
    (is (= 6 (count namespaces)))
    (is (= (namespaces :rdf) "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
    (is (= (namespaces :owl) "http://www.w3.org/2002/07/owl#"))
    (is (= (namespaces (keyword "")) "http://scap.nist.gov/vocab/test-usecase#"))
    (is (= (namespaces :xsd) "http://www.w3.org/2001/XMLSchema#"))
    (is (= (namespaces :rdfs) "http://www.w3.org/2000/01/rdf-schema#"))
    (is (= (namespaces :mapping) "http://nist.gov/secaut/vocab/mapping#"))))

(deftest test-find-classes []
  (let [classes (find-classes (create-xml-ontology file-loc))]
    (is (every? #{"#Asset" "#ProductClass" "#Checklist"} (map #(% :class) classes)))
    (is (every? empty? (map #(% :preds) classes)))))

(deftest test-find-object-properties []
  (let [preds (find-object-properties (create-xml-ontology file-loc))]
    (is (= 2 (count preds)))
    (let [hasProductClass (first (filter #(= (% :id) "hasProductClass") preds))]
      (is (= :owl:ObjectProperty (hasProductClass :type)))
      (is (= "#Asset" (first (hasProductClass :domain))))
      (is (= "#ProductClass" (first (hasProductClass :range)))))
    (let [securesProduct (first (filter #(= (% :id) "securesProduct") preds))]
      (is (= :owl:ObjectProperty (securesProduct :type)))
      (is (= "#Checklist" (first (securesProduct :domain))))
      (is (= "#ProductClass" (first (securesProduct :range)))))))

(deftest test-find-data-properties []
  (let [preds (find-data-properties (create-xml-ontology file-loc))]
      (is (= 3 (count preds)))
      (let [hasCpeName (first (filter #(= (% :id) "hasCpeName") preds))]
        (is (= :owl:DatatypeProperty (hasCpeName :type)))
        (is (= "#ProductClass" (first (hasCpeName :domain))))
        (is (= "http://www.w3.org/2001/XMLSchema#string" (first (hasCpeName :range)))))
      (let [hasChecklistName (first (filter #(= (% :id) "hasChecklistName") preds))
            hasChecklistId (first (filter #(= (% :id) "hasChecklistId") preds))]
        ;; hasChecklistName tests
        (is (= :owl:DatatypeProperty (hasChecklistName :type)))
        (is (= "#Checklist" (first (hasChecklistName :domain))))
        (is (= "http://www.w3.org/2001/XMLSchema#string" (first (hasChecklistName :range))))
        ;; hasChecklistId tests
        (is (= :owl:DatatypeProperty (hasChecklistId :type)))
        (is (= "#Checklist" (first (hasChecklistId :domain))))
        (is (= "http://www.w3.org/2001/XMLSchema#integer" (first (hasChecklistId :range)))))))

(deftest test-classes-and-relationships
  "test the classes-and-relationships function in owlXmlReader"
  (let [ont (reader-core/classes-and-relationships file-loc)
        classes-and-rels (ont :model)
        namespaces (ont :namespaces)
        classes (map #(% :class) classes-and-rels)]
    ;; test namespace data
    (is (= 6 (count namespaces)))
    (is (= (namespaces :rdf) "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
    (is (= (namespaces :owl) "http://www.w3.org/2002/07/owl#"))
    (is (= (namespaces (keyword "")) "http://scap.nist.gov/vocab/test-usecase#"))
    (is (= (namespaces :xsd) "http://www.w3.org/2001/XMLSchema#"))
    (is (= (namespaces :rdfs) "http://www.w3.org/2000/01/rdf-schema#"))
    (is (= (namespaces :mapping) "http://nist.gov/secaut/vocab/mapping#"))
    ;; test basic class data
    (is (= 3 (count classes-and-rels)))
    (is (every? #{"#Asset" "#ProductClass" "#Checklist"} classes))
    ;; test asset class and rels
    (let [asset (first (filter #(= (% :class) "#Asset") classes-and-rels))
          preds (asset :preds)]
      (let [hasProductClass (first (filter #(= (% :id) "hasProductClass") preds))]
        (is (= :owl:ObjectProperty (hasProductClass :type)))
        (is (= "#Asset" (first (hasProductClass :domain))))
        (is (= "#ProductClass" (first (hasProductClass :range))))))
    ;; test product class and rels
    (let [productClass (first (filter #(= (% :class) "#ProductClass") classes-and-rels))
          preds (productClass :preds)]
      (let [hasCpeName (first (filter #(= (% :id) "hasCpeName") preds))]
        (is (= :owl:DatatypeProperty (hasCpeName :type)))
        (is (= "#ProductClass" (first (hasCpeName :domain))))
        (is (= "http://www.w3.org/2001/XMLSchema#string" (first (hasCpeName :range))))))
    ;; test checklist class and rels
    (let [checklist (first (filter #(= (% :class) "#Checklist") classes-and-rels))
          preds (checklist :preds)]
      (let [hasChecklistName (first (filter #(= (% :id) "hasChecklistName") preds))
            hasChecklistId (first (filter #(= (% :id) "hasChecklistId") preds))
            securesProduct (first (filter #(= (% :id) "securesProduct") preds))]
        ;; hasChecklistName tests
        (is (= :owl:DatatypeProperty (hasChecklistName :type)))
        (is (= "#Checklist" (first (hasChecklistName :domain))))
        (is (= "http://www.w3.org/2001/XMLSchema#string" (first (hasChecklistName :range))))
        ;; hasChecklistId tests
        (is (= :owl:DatatypeProperty (hasChecklistId :type)))
        (is (= "#Checklist" (first (hasChecklistId :domain))))
        (is (= "http://www.w3.org/2001/XMLSchema#integer" (first (hasChecklistId :range))))   
        ;; securesProduct tests
        (is (= :owl:ObjectProperty (securesProduct :type)))
        (is (= "#Checklist" (first (securesProduct :domain))))
        (is (= "#ProductClass" (first (securesProduct :range))))))))



