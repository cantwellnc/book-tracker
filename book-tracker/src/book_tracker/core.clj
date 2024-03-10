(ns book-tracker.core
  (:require [clj-http.client :as client]
            [hickory.core :as hickory]
            [com.rpl.specter :as specter]))
            

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))



(comment
  (def resp (client/get "https://www.goldenfigbooks.com/search/site/gaiman"))
  resp
  (def parsed-resp (hickory.core/parse (:body resp)))

  (def resp-data (hickory.core/as-hickory parsed-resp))

  (:content resp-data)


  (cons (empty `()) `())



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

  resulting-titles

  (specter/select [specter/ALL list?] resulting-titles)




  )