(ns url-normalizor.test.core
  (:use [url-normalizor.core] :reload)
  (:use [clojure.test]))

(deftest test-url-equal?
  (is (url-equal? "http://jaydonnell.com" "http://jaydonnell.com"))
  (is (url-equal? "http://jaydonnell.com/" "http://jaydonnell.com"))
  (is (url-equal? "http://jaydonnell.com:80" "http://jaydonnell.com")))

(deftest test-canonical-url
  (let [want "http://jaydonnell.com/"
        tests ["http://jaydonnell.com"
               "http://jaydonnell.com:80"
               "http://Jaydonnell.com"
               "Http://Jaydonnell.com"
               "http://jaydonnell.com#blah"]]
    (doall (map #(is (= want (canonicalize-url %))) tests))
   ))



(def pace-tests [ 
         false "http://:@example.com/"
         false "http://@example.com/"
         false "http://example.com"
         false "HTTP://example.com/"
         false "http://EXAMPLE.COM/"
         false "http://example.com/%7Ejane"
         false "http://example.com/?q=%C7"
         false "http://example.com/?q=%5c"
         false "http://example.com/?q=C%CC%A7"
         false "http://example.com/a/../a/b"
         false "http://example.com/a/./b"
         false "http://example.com:80/"
         true  "http://example.com/"

         true  "http://example.com/~jane"
         true  "http://example.com/a/b"
         true  "http://example.com:8080/"
         true  "http://user:password@example.com/"
         ;; from rfc2396bis
         true  "http://www.ietf.org/rfc/rfc2396.txt"
         true  "telnet://192.0.2.16:80/"
         ;; other
         true  "http://127.0.0.1/"
         false "http://127.0.0.1:80/"
         false "http://example.com:081/"
 ])

(deftest test-pace-tests
  (doall
    (map 
     (fn [[expected url]]
       (is (= (= url (canonicalize-url url)) expected)
           (str url " normalized incorrectly")))
     (partition 2 pace-tests))))

;; mnot test suite; three tests updated for rfc2396bis.
(def mnot-tests 
[
        "/foo/bar/."                    "/foo/bar/"
        "/foo/bar/./"                   "/foo/bar/"
        "/foo/bar/.."                   "/foo/"
        "/foo/bar/../"                  "/foo/"
        "/foo/bar/../baz"               "/foo/baz"
        "/foo/bar/../.."                "/"
        "/foo/bar/../../"               "/"
        "/foo/bar/../../baz"            "/baz"
        "/foo/bar/../../../baz"         "/baz" ;;was: "/../baz"
        "/foo/bar/../../../../baz"      "/baz"
        "/./foo"                        "/foo"
        "/../foo"                       "/foo" ;;was: "/../foo"
        "/foo."                         "/foo."
        "/.foo"                         "/.foo"
        "/foo.."                        "/foo.."
        "/..foo"                        "/..foo"
        "/./../foo"                     "/foo" ;;was: "/../foo"
        "/./foo/."                      "/foo/"
        "/foo/./bar"                    "/foo/bar"
        "/foo/../bar"                   "/bar"
        "/foo//"                        "/foo/"
        "/foo///bar//"                  "/foo/bar/"
        "http://www.foo.com:80/foo"     "http://www.foo.com/foo"
        "http://www.foo.com:8000/foo"   "http://www.foo.com:8000/foo"
        "http://www.foo.com./foo/bar.html" "http://www.foo.com/foo/bar.html"
        "http://www.foo.com.:81/foo"    "http://www.foo.com:81/foo"
        "http://www.foo.com/%7ebar"     "http://www.foo.com/~bar"
        "http://www.foo.com/%7Ebar"     "http://www.foo.com/~bar"
        "ftp://user:pass@ftp.foo.net/foo/bar" 
          "ftp://user:pass@ftp.foo.net/foo/bar"
        "http://USER:pass@www.Example.COM/foo/bar" 
          "http://USER:pass@www.example.com/foo/bar"
        "http://www.example.com./"      "http://www.example.com/"
        "-"                             "-"
 ])

(deftest test-mnot-tests
  (doall
    (map 
     (fn [[original normalized]]
       (is (= normalized (canonicalize-url original)) 
           (str original " normalized should be " normalized)))
     (partition 2 mnot-tests))))


(comment "these tests don't pass (yet)")
(def failing-tests [
        true  "http://example.com/?q=%C3%87"
        true  "http://example.com/?q=%E2%85%A0"
        true  "http://example.com/?q=%5C"
        ;; from rfc2396bis
        true  "ftp://ftp.is.co.za/rfc/rfc1808.txt"
        true  "ldap://[2001:db8::7]/c=GB?objectClass?one"
        true  "mailto:John.Doe@example.com"
        true  "news:comp.infosystems.www.servers.unix"
        true  "tel:+1-816-555-1212"
        true  "urn:oasis:names:specification:docbook:dtd:xml:4.1.2"
        ;; other
        true  "http://www.w3.org/2000/01/rdf-schema#"
  ])

;; (deftest test-failing-tests
;;   (doall
;;     (map 
;;      (fn [[expected url]]
;;        (is (= (= url (canonicalize-url url)) expected)
;;            (str url " normalized incorrectly")))
;;      (partition 2 failing-tests))))

