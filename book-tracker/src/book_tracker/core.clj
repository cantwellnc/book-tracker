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
  ;; notice the query string format: a book title => a+book+title
  (def resp (client/get "https://openlibrary.org/search.json?q=good+omens"))
  (def resp-json (cheshire/decode (:body resp)))

  ;; get isbns for first item on page (assume top result for now, but in theory we could return a 
  ;; list to the user)
  (def isbns (get (first (get resp-json "docs")) "isbn"))


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

  (def my-isbns (english-published-isbns isbns))

  ;; go to golden fig + see if the book is there... 
  (defn golden-fig-entry-for
    [{:keys [url] :as isbn-map}]
    (try
      (assoc isbn-map :resp (client/get url))
      (catch Exception e
        (if (not= 404 (:status (ex-data e)))
          (throw (ex-info "HTTP request failed for some reason other than not being able to find the book..." {:ex e}))
          (assoc isbn-map :resp nil)))))

   ;; now I need a top-level data object that maps isbn-url -> response
  (def urls (map #(str "https://www.goldenfigbooks.com/book/" %) my-isbns))

  (defn url->resp-map
    [url]
    (zipmap [:url :resp] [url nil]))

  (defn valid-book-responses [url-resps]
    (filter #(some? (:resp %)) url-resps))

  ;; this gives me all the {:url ... :resp ...} pairs that are actually present on gf's website.
  (def valid-links (->> (map url->resp-map urls)
                        (pmap golden-fig-entry-for)
                        (valid-book-responses)))

  (def resp (first valid-links))

  (:url resp)

  (def as-hickory (-> resp
                      :resp
                      :body
                      hickory.core/parse
                      hickory.core/as-hickory)) 
  


  ;; TODO: trying to figure out how to use walk.
  (walk/walk (fn [item]
               (when (and (map? item) (= (:attrs item) {:class "abaproduct-status"}))
                 (:content item))) identity as-hickory)
  
  (walk/walk identity identity as-hickory)
  




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