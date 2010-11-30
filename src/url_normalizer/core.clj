
(ns url-normalizer.core
  (:require [clojure.contrib.str-utils2 :as su])
  (:import [java.net URI URL]))

(def default-port
{
 "ftp" 21
 "telnet" 23
 "http" 80
 "gopher" 70
 "news" 119
 "nntp" 119
 "prospero" 191
 "https" 443
 "snews" 563
 "snntp" 563
})

(defn normalize-port [uri]
  (let [scheme (.getScheme uri)
        port (.getPort uri)]
    (if (or (nil? port) 
            (= port -1) 
            (and (contains? default-port scheme)
                 (= port (default-port scheme))))
      nil
      (str ":" port))))

(defn normalize-path-dot-segments [uri]
  (if-let [path (.getPath uri)]
   (let [segments (su/split path #"/" -1)
         ;;x (prn segments)
         ;; resolve relative paths
         segs2 (reduce 
                (fn [acc segment]
                  (cond
                   (= "" segment ) (if (> (count acc) 0)
                                     acc
                                     (concat acc [segment]))
                   (= "."  segment) acc 
                   (= ".." segment) (if (> (count acc) 1)
                                      (drop-last acc)
                                      acc)
                   true (concat acc [segment]) 
                   )) [] segments)
         ;; add a slash if the last segment is "" "." ".."
         new-segments (if (contains? #{"" "." ".."} (last segments))
                        (concat segs2 [nil])
                        segs2)]
     (su/join "/" new-segments))))

(defn only-percent-encode-where-essential [path]
  (comment "Where is it non-essential besides tilde ~ ?. a bit of a hack, will extend as new test cases are presented" )
  (su/replace path #"(?i:%7e)" "~"))

(defn normalize-path [uri]
  (let [path (normalize-path-dot-segments uri)
        path2 (only-percent-encode-where-essential path)]
    ;; (if (or (= path "") (= path "/")) "" path)
    path2))

(defn normalize-host [uri]
  (if-let [host (.getHost uri)]
    (let [lhost (su/lower-case host)]
      (if (= (last (seq lhost)) \.) 
        (su/join "" (drop-last (seq lhost)))
        lhost))))

(defn normalize-scheme [uri]
  (if-let [scheme (.getScheme uri)]
    (su/lower-case scheme)))

(defn normalize-auth [uri]
  (let [user-info (.getUserInfo uri)]
    (if (and user-info
             (not (contains? #{"" ":"} user-info))) 
      (str user-info "@") 
      "")))

(defn normalize-query [uri] ;; TODO
  (if-let [q (.getQuery uri)] 
    (str "?" q)))

(defmulti to-uri class)
(defmethod to-uri URL [url] 
   (URI. (.getProtocol url)
         (.getUserInfo url)
         (.getHost url)
         (.getPort url)
         (.getPath url)
         (.getQuery url)
         (.getRef url)))
;; (defmethod to-uri String [url]
;;  (to-uri (URL. url)))

(defmulti canonicalize-url class)
(defmethod canonicalize-url URI [uri]
 (let [scheme (normalize-scheme uri)
       scheme-connector (if scheme "://" "")
       auth  (normalize-auth uri)
       host  (normalize-host uri)
       port  (normalize-port uri)
       path  (normalize-path uri) 
       query (normalize-query uri)]
    (str scheme scheme-connector auth host port path query)))
(defmethod canonicalize-url URL [url] (canonicalize-url (to-uri url)))
(defmethod canonicalize-url String [url]
  (try 
    (canonicalize-url (to-uri (URL. url)))
    (catch java.net.URISyntaxException    e (canonicalize-url (URI. url)))
    (catch java.net.MalformedURLException e (canonicalize-url (URI. url)))
    ))

(defmulti url-equal? (fn [a b] [(class a) (class b)]))

(defmethod url-equal? [String String] [url1 url2]
           (let [u1 (canonicalize-url (URI. url1))
                 u2 (canonicalize-url (URI. url2))]
             (= u1 u2)))
