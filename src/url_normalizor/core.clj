
(ns url-normalizor.core
  (:require [clojure.contrib.str-utils2 :as su])
  (:import [java.net URI]))

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
  (let [port (.getPort uri)]
    (if (or (nil? port) (= port -1) (= port 80))
      nil
      (str ":" port))))

(defn normalize-path [uri]
  (let [path (.getPath uri)]
    (if (or (= path "") (= path "/"))
      ""
      path)))

(defn normalize-host [uri]
  (if-let [host (.getHost uri)]
    (su/lower-case host)))

(defn normalize-scheme [uri]
  (if-let [scheme (.getScheme uri)]
    (su/lower-case scheme)))

(defmulti canonicalize-url class)
(defmethod canonicalize-url URI [uri]
 (let [scheme (normalize-scheme uri)
       scheme-connector (if scheme "://" "")
       host  (normalize-host uri)
       port  (normalize-port uri)
       path  (normalize-path uri) 
       query (.getQuery uri)]
    (str scheme scheme-connector host port path query)))
(defmethod canonicalize-url String [url]
           (canonicalize-url (URI. url)))

(defmulti url-equal? (fn [a b] [(class a) (class b)]))

(defmethod url-equal? [String String] [url1 url2]
           (let [u1 (canonicalize-url (URI. url1))
                 u2 (canonicalize-url (URI. url2))]
             (= u1 u2)))

(comment  1. DONE Normalizing the port
          2. DONE Case insensitive shceme and authority
          3. DONE "http://jaydonnell.com/" and "http://jaydonnell.com" are the same
          4. Decode unreserved characters
          5. DONE Remove fragments)
