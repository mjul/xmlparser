(ns com.mjul.xmlparser.parser-test
  (:use [com.mjul.xmlparser.parser] :reload-all)
  (:use [name.choi.joshua.fnparse])
  (:use [clojure.test]))

(defn tokenize [s] (vec s))
(defn parse-str [prod-fn input] (first (prod-fn (struct state-s (tokenize input) 0 0))))

(deftest characters-test
  (testing "2.2 Characters"
    (testing "[2] Char-lit"
      (testing "With valid input"
	(are [match input] (= match (parse-str Char-lit input))
	     \a "a"
	     \b "b"
	     \1 "123")))
    (testing "[2a] RestrictedChar-lit"
      (testing "With valid input"
	(are [match input] (= match (parse-str RestrictedChar-lit input))
	     \u0080 "\u0080"
	     \u0080 "\u0080\u0081")))))

(deftest common-syntactic-constructs-test
  (testing "2.3 Common Syntactic Constructs"
    (testing "Whitespace"
      (testing "[3] S-lit"
	  (are [match input] (= match (parse-str S-lit input))
	       [\space] " "
	       [\space \space] "  "
	       [\tab]   (str \tab) 
	       [\return] (str \return)
	       [\space \tab \return] (str \space \tab \return))))
    (testing "Names and Tokens"
      (testing "[4] NameStartChar-lit"
	(testing "With valid input"
	  (are [match input] (= match (parse-str NameStartChar-lit input))
	       \a "a"
	       \A "A"
	       \_ "_abc")))
      (testing "[4a] NameChar-lit"
	(testing "With valid input"
	  (are [match input] (= match (parse-str NameChar-lit input))
	       \a "a"
	       \a "a-b"
	       \1 "123 ")))
      (testing "[5] Name"
	(testing "With valid input"
	  (are [match input] (= match (parse-str Name input))
	       (create-token :Name "foo") "foo"
	       (create-token :Name "bar123") "bar123")))
      (testing "[6] Names"
	(testing "With valid input"
	  (are [match input] (= match (parse-str Names input))
	       (create-token :Names ["foo" "bar"]) "foo bar"
	       (create-token :Names ["bar123" "a-b.c"]) "bar123 a-b.c")))
      (testing "[7] Nmtoken"
	(testing "With valid input"
	  (are [match input] (= match (parse-str Nmtoken input))
	       (create-token :Nmtoken "foo") "foo bar"
	       (create-token :Nmtoken "bar123") "bar123 a-b.c")))
      (testing "[8] Names"
	(testing "With valid input"
	  (are [match input] (= match (parse-str Nmtokens input))
	       (create-token :Nmtokens ["foo" "bar"]) "foo bar"
	       (create-token :Nmtokens ["bar123" "a-b.c"]) "bar123 a-b.c"))))))