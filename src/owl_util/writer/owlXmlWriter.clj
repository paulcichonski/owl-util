(ns owl-util.writer.owlXmlWriter
  ^{:author "Paul Cichonski"
    :doc "Writes OWL models in RDF/XML

           work-in-progress"}
  (:require [clojure.data.xml :as xml]))


  (comment "currently just sketching out the struture, none of this is legit yet. possibly build off this: 
http://java.dzone.com/articles/clojure-creating-xml-document or this: http://dev.clojure.org/display/DXML/Fuller+XML+support")


  
(defn gen-classes [classes namespaces]
  (for [class classes]
    (xml/element :owl:Class {:rdf:ID class}
                 (xml/element :rdfs:subClassOf {:rdf:resource "http://www.w3.org/2002/07/owl#Thing"}))))

(defn gen-preds [preds namespaces]
  (for [pred preds]
    (let [domain-elements (for [domain (pred :domain)]
                            (xml/element :rdfs:domain {:rdf:resource domain}))
          range-elements (for [range (pred :range)]
                           (xml/element :rdfs:range {:rdf:resource range}))
          aggregate (concat domain-elements range-elements)]
      ; need to do it this way b/c xml/element only accepts 0-n individual elements, not a seq of them
      (apply xml/element (pred :type) {:rdf:ID (pred :id)} aggregate))))

(defn gen-rdf-xml [elements namespaces]
  (apply xml/element :rdf:RDF {}
               elements))

(defn write-owl [classes-and-relationships file-loc]
  (let [namespaces (classes-and-relationships :namespaces)
        model (classes-and-relationships :model) 
        class-elements (gen-classes (map #(% :class) model) namespaces)
        pred-elements (gen-preds (flatten (map #(% :preds) model)) namespaces)
        xml-doc (gen-rdf-xml (concat class-elements pred-elements) namespaces)]
    (with-open [out-file (java.io.FileWriter. file-loc)]
        (xml/emit xml-doc out-file))))
