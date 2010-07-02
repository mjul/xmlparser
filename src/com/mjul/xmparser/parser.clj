(ns com.mjul.xmparser.parser
  (:use name.choi.joshua.fnparse clojure.contrib.error-kit
        [clojure.contrib.seq-utils :only [flatten]]))

(defstruct state-s :remainder :column :line)
(defn lit-char-range [start end] 
  (term #(<= (int start) (int %) (int end))))

(defstruct token :type :value)
(def create-token (partial struct token))

;;EBNF productions for XML 1.1 grammar taken from http://www.w3.org/TR/xml11/

;;[1]   	document	   ::=   	 ( prolog element Misc* ) - ( Char* RestrictedChar Char* )
;;(declare prolog element Misc)
;; (def document (except (conc prolog element (rep* Misc)) 
;; 		      (conc (rep* Char) RestrictedChar (rep* Char)))

;; [2]   	Char	   ::=   	[#x1-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
;; [2a]   	RestrictedChar	   ::=   [#x1-#x8] | [#xB-#xC] | [#xE-#x1F] | [#x7F-#x84] | [#x86-#x9F]
(def Char-lit (alt (lit-char-range 0x01 0xD7FF)
		   (lit-char-range 0xE000 0xFFFD)
		   (lit-char-range 0x10000 0x10FFFF)))

(def RestrictedChar-lit (alt (lit-char-range 0x0001 0x0008) 
			     (lit-char-range 0x000B 0x000C) 
			     (lit-char-range 0x000E 0x001F) 
			     (lit-char-range 0x007F 0x0084) 
			     (lit-char-range 0x0086 0x009F)))

;; [3]   	S	   ::=   	(#x20 | #x9 | #xD | #xA)+
(def S-lit (rep+ (lit-alt-seq [\u0020 \u0009 \u000D \u000A])))

;; [4]   	NameStartChar	   ::=   	":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] 
;;                                      | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] 
;;                                      | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] 
;;                                      | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] 
;;                                      | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
(def NameStartChar-lit (alt (lit \:) 
			    (lit-char-range \A \Z) 
			    (lit \_) 
			    (lit-char-range \a \z) 
			    (lit-char-range 0x00C0 0x00D6) 
			    (lit-char-range 0x00D8 0x00F6)
			    (lit-char-range 0x00F8 0x02FF) 
			    (lit-char-range 0x00370 0x037D) 
			    (lit-char-range 0x0037F 0x1FFF) 
			    (lit-char-range 0x00200C 0x200D) 
			    (lit-char-range 0x002070 0x218F) 
			    (lit-char-range 0x002C00 0x2FEF) 
			    (lit-char-range 0x003001 0xD7FF) 
			    (lit-char-range 0x00F900 0xFDCF) 
			    (lit-char-range 0x00FDF0 0xFFFD) 
			    (lit-char-range 0x0010000 0xEFFFF)))

;; [4a]   	NameChar	   ::=   	 NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
(def NameChar-lit (alt NameStartChar-lit (lit \-) (lit \.) 
		       (lit-char-range \0 \9) 
		       (lit \u00B7) 
		       (lit-char-range 0x0300 0x036F) 
		       (lit-char-range 0x203F 0x2040)))

;; [5]   	Name	   ::=   	 NameStartChar (NameChar)*
(def Name (semantics (conc NameStartChar-lit (rep* NameChar-lit))
		     #(let [[nsc nc-seq] %] (create-token :Name (str nsc (apply str nc-seq))))))

(defn values-from-separated-list [token separator-token-seq]
  (vec (map :value (cons token (map second separator-token-seq)))))
  
;; [6]   	Names	   ::=   	 Name (#x20 Name)*
(def Names (semantics (conc Name (rep* (conc (lit \u0020) Name)))
		      #(let [[name space-name-seq] %]
			 (create-token :Names (values-from-separated-list name space-name-seq)))))

;; [7]   	Nmtoken	   ::=   	(NameChar)+
(def Nmtoken (semantics (rep+ NameChar-lit)
			#(create-token :Nmtoken (apply str %))))

;; [8]   	Nmtokens	   ::=   	 Nmtoken (#x20 Nmtoken)*
(def Nmtokens (semantics (conc Nmtoken (rep* (conc (lit \u0020) Nmtoken)))
			 #(let [[nmt space-nmt-seq] %]
			    (create-token :Nmtokens (values-from-separated-list nmt space-nmt-seq)))))


