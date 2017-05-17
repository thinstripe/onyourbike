(ns onyourbike
  (:gen-class)
  (:require
    [clojure.string :as string]
    [clojure.data.json :as json]
    [yada.yada :as yada]
    [bidi.bidi :refer [tag]]
    [hiccup.core :refer [html]]))

(defn- abs
  "Returns the absolute value of the given value n"
  [n]
  (if (neg? n) (- n) n))

(defn- nearest
  "Nearest coordinate comparator"
  [{:keys [latitude longitude]} {:strs [lat lon]}]
  (+ (abs (- latitude lat)) (abs (- longitude lon))))

(defn- to-bikepoint-line
  "Extracts the Boris-bike point data from raw json map"
  [{:strs [commonName lat lon additionalProperties]}]
  (let [nbBikes (first (filter #(= "NbBikes" (get % "key")) additionalProperties))
        numBikes (get nbBikes "value")]
    {:name commonName :lat lat :lon lon :num numBikes}))

(defn- nearest-bikepoints
  "Given a location finds the nearest n Boris-bike points"
  [location n]
  (->>
    (slurp "https://api.tfl.gov.uk/BikePoint?app_id=a11ead08&app_key=b913ea59d79a3e545b9e09f933d0912b")
    (json/read-str)
    (sort-by (partial nearest location))
    (take n)
    (map to-bikepoint-line)))

(defn- mkstr
  "Constructs a string using a separator and any remaining arguments"
  [sep & strings]
  (string/join sep strings))

(defn- bikepoint-routes-json
  "Returns the nearest Boris-bike points for a given location"
  []
  (yada/resource
    {:id :onyourbike.resources/bikepoints.json
     :methods
         {:get
          {:parameters {:query {:latitude String :longitude String :nearest String}}
           :produces   "text/json"
           :response   (fn [ctx]
                         (let [q (-> ctx :parameters :query)
                               bd #(vector %2 (BigDecimal. #^String (%2 %1)))
                               location (into {} [(bd q :latitude) (bd q :longitude)])
                               n (Integer. #^String (:nearest q))
                               bikepoints (nearest-bikepoints location n)]
                           (json/json-str bikepoints)))}}}))

(defn- bikepoint-routes-html
  "Returns an access controlled resource which shows the nearest n Boris-bike points for a given location"
  [location n]
  (yada/resource
    {:id :onyourbike.resources/bikepoints.html
     :access-control
         {:scheme        "Basic"
          :verify        (fn [[user password]]
                           (when (= [user password] ["juxt" "letmein"])
                             {:user user :roles #{:user}}))
          :authorization {:methods {:get :user}}}
     :methods
         {:get
          {:produces "text/html"
           :response (fn [ctx]
                       (let [title (format "Nearest %d Boris-bike points around Leyton (%f,%f)" n (:latitude location) (:longitude location))]
                         (html
                           [:head [:title title]
                            [:style (mkstr "\n"
                                           "table,th,td {border:1px solid black; border-collapse:collapse}"
                                           "th,td {padding:5px}"
                                           "tr:hover {background-color:#f5f5f5}")]]
                           [:body
                            [:h1 title]
                            [:table
                             [:thead [:tr [:th "Name"] [:th "Latitude"] [:th "Longitude"] [:th "# Bikes"]]]
                             (into [:tbody]
                                   (for [{:keys [name lat lon num]} (nearest-bikepoints location n)]
                                     [:tr [:td [:pre name]] [:td lat] [:td lon] [:td num]]
                                     ))]])))}}}))

(defn- web-server
  "Starts the web server"
  [options]
  (let [leyton {:latitude 51.561947 :longitude -0.013139}
        bikepoints-html (bikepoint-routes-html leyton 5)
        bikepoints-json (bikepoint-routes-json)]
    (yada/listener
      ["" [
           ["/" bikepoints-html]
           ["/bikepoints" bikepoints-json]
           ["/api" (-> ["/bikepoints" bikepoints-json]
                       (yada/swaggered
                         {:info     {:title       "Nearest Boris-bike points to a given location"
                                     :version     "1.0"
                                     :description "An API to provide the nearest Boris-bike points to a given location"}
                          :basePath "/api"})
                       (tag :onyourbike.resources/api))]
           [true (yada/handler nil)]]]
      options)))

(defn -main [& args]
  (web-server {:port 8080})
  @(promise))
