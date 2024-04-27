(ns book-tracker.core
  (:require [clj-http.client :as client]
            [hickory.core :as hickory]
            [clojure.string :as string]
            [cheshire.core :as json]
            [ring.adapter.jetty :refer [run-jetty]]
            [postal.core :as postal]))



(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))



(comment
  `(defn english-published-isbns
     "get the english-speaking isbns (which start with 9780 or 9781,
     ref: https://en.wikipedia.org/wiki/ISBN, see 'Registration Element')
     "
     [isbns]
     (->> isbns
          (filter #(or
                    (string/starts-with? % "9781")
                    (string/starts-with? % "9780")))
          (map parse-long)
          sort))



  ;; go to golden fig + see if the book is there... 
  (defn golden-fig-entry-for
    [{:keys [url] :as isbn-map}]
    (try
      (assoc isbn-map :resp (client/get url))
      (catch Exception e
        (if (not= 404 (:status (ex-data e)))
          (throw (ex-info "HTTP request failed for some reason other than not being able to find the book..." {:ex e}))
          (assoc isbn-map :resp nil)))))


   ;; now I need a map that maps isbn-url -> response. 
  (defn url->book-map
    [url]
    (zipmap [:url :resp] [url nil]))

  (defn valid-book-responses
    "Collects the book response maps from golden fig
     that have a non-nil :resp (response) (i.e. golden fig recognizes
     the isbn and at least has the capacity to order the book
     for you if it's not in the store)."
    [url-resps]
    (filter #(some? (:resp %)) url-resps))


  (defn add-string-resp-body
    [book-map]
    (assoc book-map :string-resp-body (str (get-in book-map [:resp :body]))))

  ;; This takes a valid link + returns a map that tells us if it's in stock at ANY store. 
  (defn add-in-store?-info
    "Takes a book-map and returns a new map that tells us if a book is
     in stock at SOME golden fig location. It also preserves some of the original data, like the :url and the stringified response body."
    [book-map]
    (let [book-map-with-str-resp (->> book-map
                                      add-string-resp-body)
          _in-store? (comp #(nil? (re-find #"NOT CURRENTLY IN THE STORE" %)) :string-resp-body)]

      (if (_in-store? book-map-with-str-resp)
        {:in-store true
         :url (:url book-map-with-str-resp)
         :string-resp-body (:string-resp-body book-map-with-str-resp)}
        {:in-store false})))


  (def known-store-locations ["Carrboro" "Durham"])

  (defn in-store-location?
    "If a book is in `store-location`, returns the location,
     else nil."
    [book-map store-location]
    (let [present? (->> book-map
                        :string-resp-body
                        (re-find (re-pattern (str "<span class=\"abaproduct-lsi-outlet-name\">" store-location  "</span>"))))]
      (when present?
        store-location)))

  (defn find-and-add-store-name
    "adds the golden fig store location(s) where the book is present to the book-map."
    [book-map]
    (->> (map (partial in-store-location? book-map)
              known-store-locations)
         (filter some?)
         (into [])
         (assoc book-map :store-locations)))



  ;; ACTUALLY USING THE FNS!

  ;; notice the query string format: a book title => a+book+title  
  (def resp (client/get "https://openlibrary.org/search.json?q=good+omens"))

  (def resp-json (json/decode (:body resp)))

  ;; get isbns for FIRST item on page (assume top result for now, but in theory we could return a 
  ;; list to the user)
  (def isbns (get (first (get resp-json "docs")) "isbn"))

  (def my-isbns (english-published-isbns isbns))


  (def urls (map #(str "https://www.goldenfigbooks.com/book/" %) my-isbns))

  ;; this gives me all the {:url ... :resp ...} book-maps that are actually present on golden fig's website.
  (def valid-links (->> urls
                        (map url->book-map)
                        (map golden-fig-entry-for)
                        (valid-book-responses)))

  ;; For all valid links, determine if the book is in any store (and add that data to the map), and then 
  ;; determine the particular store. We end up with a map that contains all this info for the books that
  ;; are actually present in a particular store. The book maps that DO NOT satisfy the above are filtered out.
  (def in-store-copies (->>  valid-links
                             (map add-in-store?-info)
                             (filter :in-store)
                             (map find-and-add-store-name)))

  in-store-copies



  (client/get "https://www.goldenfigbooks.com/book/9780060853976")
  ;; Ideally we have our in-store-copies by now. This should be a seq of maps like 
  (def in-store-books '({:url "https://www.goldenfigbooks.com/book/9780060853976"
                         :resp {:body "stuff"} ;; plus lots of other keys pertaining to the request
                         :string-resp-body "a messy string containing the html of the body from the call to "
                         :store-locations ["Carrboro"]}))

  (defn location-message-for
    [book-map]
    (let [stores (:store-locations book-map)]
      (cond
        (= 1 (count stores))
        (str "Your book is ready at Golden Fig " (first stores) "!")

        :else
        (str "Your book is ready at Golden Fig " (first stores) "and " (second stores) "!"))))


  (defn email-body-for
    "Builds the email body for a book-map. Hardcoded to me currently."
    [book-map]
    {:from "noreply@book-tracker.com"
     :to ["cantwell.nc@gmail.com"]
     :subject (location-message-for book-map)
     :body (str "Your book is in stock! Here's the link to purchase it online: " (:url book-map))})



  (defn notify
    "Sends an email using postal to whoever submitted the notification request
     Currently only supports gmail, because that's the only email server I know lol"
    [message]
    (postal/send-message {:host "smtp.gmail.com"
                          :user (System/getenv "EMAIL_USER")
                          :pass (System/getenv "EMAIL_PASS") 
                          :port 587
                          :tls  true} message))

  (notify (email-body-for (first in-store-books)))

  )

;; (client/get "http://localhost:3000")


(defn handler [request]
  {:status 200
   :headers {"Content-Type" "text/plain"}
   :body (json/encode {:url "https://www.goldenfigbooks.com/book/9780060853976"
                       :resp {:body "stuff"} ;; plus lots of other keys pertaining to the request
                       :string-resp-body "a messy string containing the html of the body from the call to "
                       :store-locations ["Carrboro"]})})

;; (run-jetty handler {:port 3000})
