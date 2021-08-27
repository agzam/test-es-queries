(ns script
  (:require [clojure.edn :as edn]
            [babashka.curl :as curl]
            [cheshire.core :as json]
            [clojure.pprint :refer [pprint]])
  #_(:import java.util.Base64))

(def data (-> "queries.edn" slurp edn/read-string))

(defn make-url [es-ver & rest]
  (str "http://localhost:"
   (get-in data [:versions (-> es-ver str keyword) :port])
   "/" (clojure.string/join "/" rest)))

(defn delete-indices [es-ver index-mapping]
  (let [index-names (if index-mapping
                      (map name (keys index-mapping))
                      (map name (keys (get-in data [:default-index-mapping (-> es-ver str keyword)]))))]
    (doseq [idx index-names]
      ;; (println "deleting" (make-url es-ver idx))
      (curl/delete (make-url es-ver idx) {:throw false}))))

#_(defn encode [to-encode]
  (.encodeToString (Base64/getEncoder) (.getBytes to-encode)))

(defn create-indices [es-ver index-mapping]
  (doseq [[idx-k idx-val] (if index-mapping
                            (get index-mapping (-> es-ver str keyword))
                            (get-in data [:default-index-mapping (-> es-ver str keyword)]))]
    ;; (println "creating index" (make-url es-ver (name idx-k)))
    (curl/put
     (make-url es-ver (name idx-k))
     {:body idx-val
      #_#_:headers {"Authorization" (str "Basic " (-> "es-pass.txt" slurp encode))}})))

(defn create-documents [es-ver documents]
  (doseq [[idx docs] (if documents
                       documents
                       (get data :default-documents))
          :let [url (if (= es-ver 5)
                      (make-url es-ver (name idx) "1")
                      (make-url es-ver (name idx) "_doc"))]]
    (doseq [doc docs]
      ;; (println "pushing document" url)
      (curl/post
       url
       {:headers {"Content-Type" "application/json"}
        :body    (json/generate-string doc)}))))

(defn send-query [es-ver index-mapping query]
  (let [ver-k (-> es-ver str keyword)
        indices (if index-mapping
                  (->> (get index-mapping ver-k)
                       keys
                       (map name))
                  (->> (get-in data [:default-index-mapping ver-k])
                       keys
                       (map name)))]
    (doseq [idx indices
            :let [url (make-url es-ver idx "_search")
                  payload (json/generate-string {:query query})]]
      (println "\nSending query:" url)
      (pprint query)
      (println "\nResults:")
      (let [resp (curl/get
                  url
                  {:headers {"Content-Type" "application/json"}
                   :body payload
                   :throw false})]
        (pprint (json/parse-string (:body resp) true))))))

(defn check-for-dups [queries-col]
  (let [find-dups #(map key (remove (comp #{1} val) (frequencies %)))
        dups (find-dups queries-col)]
    (when (seq dups)
      (println "Duplicate quieries detected. Duplicates will be ignored")
      (pprint dups))
    (distinct queries-col)))

(let [queries (check-for-dups (:queries data))
      only (->> queries (filter #(-> % :only true?)))
      queries (if (seq only)
                (do (println "Found :only, running selectively")
                    only)
                queries)]
  (doseq [{:keys [query
                  versions
                  index-mapping
                  documents]} queries]
    (doseq [es-ver (or versions [5 7])]
      (delete-indices es-ver index-mapping)
      (create-indices es-ver index-mapping)
      (create-documents es-ver documents)
      (Thread/sleep 1000)
      (send-query es-ver index-mapping query))))
