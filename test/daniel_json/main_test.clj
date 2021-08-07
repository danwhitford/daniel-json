(ns daniel-json.main-test (:require [clojure.test :as test] [daniel-json.main :as daniel-json]))

(test/deftest atoms-tokenise
  (test/is (= [5] (daniel-json/get-tokens (daniel-json.main/string-to-pbr "5"))))
  (test/is (= ["bar"] (daniel-json/get-tokens (daniel-json.main/string-to-pbr "\"bar\""))))
  (test/is (= [true] (daniel-json/get-tokens (daniel-json.main/string-to-pbr "true"))))
  (test/is (= [false] (daniel-json/get-tokens (daniel-json.main/string-to-pbr "false")))))

(test/deftest easy-string-tokenise
  (test/is
   (= [:lcurly "foo" :colon "bar" :rcurly]
      (daniel-json/get-tokens (daniel-json.main/string-to-pbr "{\"foo\": \"bar\"}")))))

(test/deftest array-string-tokenise
  (test/is
   (= [:lbrack 1 :comma 2 :comma 3 :comma "egg" :comma true :comma false :comma 56 :rbrack]
      (daniel-json/get-tokens (daniel-json.main/string-to-pbr "[1,2,3, \"egg\", true, false, 56]")))))

(test/deftest array-string
  (test/is
   (= [1 2 3 4 ["a", "b", "c", "d"]]
      (daniel-json.main/read-str "[1,2,3,4, [\"a\", \"b\",  \"c\",   \"d\"]]"))))

(test/deftest map-string
  (test/is
   (= {"foo" ["tis a \\ big \\ string\\nanother\\tline" {"egg" "bar", "nested" [1 2 3 4 5]} ["array" "in" "array"]], "isIt" true}
      (daniel-json.main/read-str (slurp "./test/daniel_json/test.json")))))

(test/deftest read-file
  (test/is
   (= {"foo" ["tis a \\ big \\ string\\nanother\\tline" {"egg" "bar", "nested" [1 2 3 4 5]} ["array" "in" "array"]], "isIt" true}
      (daniel-json.main/read-file "./test/daniel_json/test.json"))))


(test/run-all-tests #"daniel-json")
