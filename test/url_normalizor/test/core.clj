(ns url-normalizor.test.core
  (:use [url-normalizor.core] :reload)
  (:use [clojure.test]))

(deftest test-url-equal?
  (is (url-equal? "http://jaydonnell.com" "http://jaydonnell.com"))
  (is (url-equal? "http://jaydonnell.com/" "http://jaydonnell.com"))
  (is (url-equal? "http://jaydonnell.com:80" "http://jaydonnell.com")))

(deftest test-canonical-url
  (is (= "http://jaydonnell.com" (canonicalize-url "http://jaydonnell.com")))
  (is (= "http://jaydonnell.com" (canonicalize-url "http://jaydonnell.com:80")))
  (is (= "http://jaydonnell.com" (canonicalize-url "http://Jaydonnell.com")))
  (is (= "http://jaydonnell.com" (canonicalize-url "Http://Jaydonnell.com")))
  (is (= "http://jaydonnell.com" (canonicalize-url "http://jaydonnell.com#blah"))))

