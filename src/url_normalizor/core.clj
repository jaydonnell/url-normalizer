
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

(defn normalize-path-dot-segments [uri]
  (let [path (.getPath uri)
        segments (su/split path #"/" -1)
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
    (su/join "/" new-segments)))

(defn normalize-path [uri]
  (let [path (normalize-path-dot-segments uri)]
    ;; (if (or (= path "") (= path "/")) "" path)
    path))

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
    (if user-info (str user-info "@") "")))

(defmulti canonicalize-url class)
(defmethod canonicalize-url URI [uri]
 (let [scheme (normalize-scheme uri)
       scheme-connector (if scheme "://" "")
       auth  (normalize-auth uri)
       host  (normalize-host uri)
       port  (normalize-port uri)
       path  (normalize-path uri) 
       query (.getQuery uri)]
    (str scheme scheme-connector auth host port path query)))
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
