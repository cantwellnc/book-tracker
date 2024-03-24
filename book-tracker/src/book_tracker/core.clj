(ns book-tracker.core
  (:require [clj-http.client :as client]
            [hickory.core :as hickory]
            [com.rpl.specter :as specter]
            [clojure.string :as string]
            [cheshire.core :as cheshire]
            [clojure.walk :as walk]))


(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(comment

  (defn english-published-isbns
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
  (def resp-json (cheshire/decode (:body resp)))

  ;; get isbns for FIRST item on page (assume top result for now, but in theory we could return a 
  ;; list to the user)
  (def isbns (get (first (get resp-json "docs")) "isbn"))

  (def my-isbns (english-published-isbns isbns))


  (def urls (map #(str "https://www.goldenfigbooks.com/book/" %) my-isbns))

  ;; this gives me all the {:url ... :resp ...} book-maps that are actually present on golden fig's website.
  (def valid-links (->> (map url->book-map urls)
                        (pmap golden-fig-entry-for)
                        (valid-book-responses)))

  ;; For all valid links, determine if the book is in any store (and add that data to the map), and then 
  ;; determine the particular store. We end up with a map that contains all this info for the books that
  ;; are actually present in a particular store. The book maps that DO NOT satisfy the above are filtered out.
  (def in-store-copies (->>  valid-links
                             (map add-in-store?-info)
                             (filter :in-store)
                             (map find-and-add-store-name)))

  in-store-copies










  ;; we may or may not need the response body as a data structure, so here it is
  (defn resp-body-as-hickory
    "Adds a hickory representation of the golden fig 
       reponse to the book map."
    [resp]
    (let [hickory-resp (-> resp
                           :resp
                           :body
                           hickory.core/parse
                           hickory.core/as-hickory)]
      (assoc resp :hickory-resp hickory-resp)))
  )







































(comment
  (def resp (client/get "https://www.goldenfigbooks.com/search/site/gaiman"))
  resp
  (def parsed-resp (hickory.core/parse (:body resp)))

  (def resp-data (hickory.core/as-hickory parsed-resp))

  (:content resp-data)



  (defn extract-titles [data]
    (cond
      (empty? data) data
      (map? data) (if (= {:class "search-result"} (:attrs data))
                            ;; if we've found a search result, explicitly manipulate to 
                            ;; get our titles
                    (let [search-contents (filter map? (:content data))
                          get-title #(-> %
                                         :content
                                         (nth 3)
                                         :content
                                         first)
                          title (get-title (first search-contents))]
                      title)
                    (extract-titles (:content data)))
      (vector? data) (cons (extract-titles (first data)) (extract-titles (apply vector (rest data))))))



  (def data {:type :element,
             :attrs {:class "search-result"},
             :tag :li,
             :content
             ["\n  "
              {:type :element,
               :attrs {:class "title"},
               :tag :h3,
               :content
               ["\n        "
                {:type :comment, :content [" RM-2886 ADA compliance Issue 121, 122 fixes "]}
                "\n    "
                {:type :element,
                 :attrs {:id "b-9780063358089", :href "/book/9780063358089"},
                 :tag :a,
                 :content ["What You Need to Be Warm"]}
                "\n    (Hardcover)  "]}]})


  (extract-titles (into [] (repeat 3 data)))



  (def resulting-titles (extract-titles resp-data))

  resulting-titles)