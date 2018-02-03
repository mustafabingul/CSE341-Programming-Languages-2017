; *********************************************
; *  341 Programming Languages                *
; *  Fall 2017                                *
; *  Author: Yakup Genc                       *
; *          Mustafa BİNGÜL                   *
; * 				   141044077                      *
; *********************************************
; *	Annotation :															*
; * Gen-Decoder-A ve Gen-Decoder-B0 fonksiyon *
; * larını fonksiyon return edecek şekilde		*
; * yazdım. Ancak mantıklı olmadığından 			*
; * verilen data yı alıp, decode edip				  *
; * sonrada return ettim.(değişiklik yaptım.) *
; *																						*
; *********************************************

;; utility functions 
(load-file "include.clj") ;; "c2i and "i2c"

(require '[clojure.set :as set])
(use 'clojure.java.io)

(defn read-as-list
	"Reads a file containing one word per line and returns a list of words (each word is in turn a list of characters)."
	[filename]
	; Implement this function...
	(with-open [sttr (reader filename)]
		(def mlist '( ))
		(def rlist '( ))
		(doseq [word (line-seq sttr)]
			(def mlist (conj mlist (apply list (clojure.string/split word #"")))))
		)
	(reverse mlist)
	;;'((a b c) (d e f))
	)
;; Tüm dosyayı tek bir
;; string liste yapar.
(defn readtwo [filename]
	(with-open [sttr (reader filename)]
		(def tlist '())
		(doseq [word (line-seq sttr)]
			(def tlist (conj tlist word))
			)
		)
	(reverse tlist)
	)

;; -----------------------------------------------------
;; HELPERS
;; *** PLACE YOUR HELPER FUNCTIONS BELOW ***

;; Verilen kelimeyi dictionary de arar.
;; LinearSearch algoritmasın kullanılmıştır.
;; #true yada #false return eder.
(defn spell-checker-0
	[word]

	(def searchedword (apply list (clojure.string/split word #"")))
	(let [searchlist (read-as-list "dictionary2.txt")]
		(def c (count searchlist))
		(def i (atom 0))
		(def stat false)
		(while (and (< @i c) (= stat false))
			(do
				(if (= searchedword (nth searchlist @i))
					(def stat true)
					)
				(swap! i inc)))
		)
	stat
	)
;; Verilen kelimeyi dictionary de arar.
;; BinarySearch algoritması kullanılmıştır.
;; #true yada #false return eder.
(defn spell-checker-1
	[word]
	(def mylist '())
	(let [mylist (readtwo "dictionary2.txt")]
		(def c (count mylist))                                    ;;listenin count u.
		(def startnumber 0)
		(def midnumber)
		(def lastnumber (- c 1))
		(def result false)
		(while (and (<= startnumber lastnumber) (= result false))
			(do
				(def midnumber (int (/ (+ startnumber lastnumber) 2)))
				(if (> 0 (compare word (nth mylist midnumber)))
					(def lastnumber (- midnumber 1))
					)
				(if (< 0 (compare word (nth mylist midnumber)))
					(def startnumber (+ midnumber 1))
					)
				(if (= 0 (compare word (nth mylist midnumber)))
					(def result true)
					)
				))
		)
	result
	)
;; dışarıdan alınan karakter listelerini
;; string yapar
;; '((a b c)(a b c))  >>  "abcabc"
(defn fullSentence [paragraph1]
	(def s (list))
	(def c (count paragraph1))
	(def x (atom 0))
	(while (< @x c)
		(do

			(def s (conj s (apply str (nth paragraph1 @x))))
			(swap! x inc)))
	(def s (apply str (reverse s)))
	s

	)

;; Verilen collectionun
;; tüm kombinasyonlarını yapar.
;; @Source:
;; https://stackoverflow.com/questions/27884794/
;; clojure-using-loop-and-recur-with-a-lazy-sequence
(defn permutations [s]
	(lazy-seq
		(if (seq (rest s))
			(apply concat (for [x s]
											(map #(cons x %) (permutations (remove #{x} s)))))
			[s]))
	)
;; tekrarlı harfleri belirler
;; liste return eder.
;; @Source:
;; https://stackoverflow.com/questions/
;; 12657566/idiomatic-clojure-way-to-find-
;; most-frequent-items-in-a-seq
(defn most-frequent-n [n items]
	(->> items
			 frequencies
			 (sort-by val)
			 reverse
			 (take n)
			 (map first)))
;; String de ki en çok
;; tekrar eder 6 rakamı
;; return eder.
(defn mostFrequentSixLetter [sentence1]
	(def i (atom 0))
	(def mostLetter '())
	(def sss (fullSentence sentence1))
	(while (< @i 6)
		(do
			(def mostLetter (conj mostLetter (nth (most-frequent-n 26 sss) @i)))
			(swap! i inc))
		)
	(reverse mostLetter)
	)
;; Verilen stringi
;; karakter listesine çevirir.
;; "abc def"  >>  '((a b c)(d e f))
(defn sentenceToList [string]
	(def firstlist (apply list (clojure.string/split string #" ")))
	(def c (count firstlist))
	(def generallist (list))
	(def i (atom 0))
	(while (< @i c)
		(do
			(def tmplist (apply list (clojure.string/split (nth firstlist @i) #"")))
			(def generallist (conj generallist tmplist))
			(swap! i inc)))
	(reverse generallist))

;; -----------------------------------------------------
;; DECODE FUNCTIONS

(defn Gen-Decoder-A
	[paragraph]
	(def sntnc (list))
	(def sortedalphabet  '(\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z))
	(def sortedalphabett  '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
	(def mappinglist '())
	(def mappinglist (permutations sortedalphabet))
	(def markedlist '())

	(def index 0)
	(def i (atom 0))
	(def s1 false)
	(def s2 false)
	(def paragraphcount (count paragraph))                     ;; ((...)(....)) --count.

	(while (= s1 false)
		(do

			(def k (atom 0))
			(def markedlist (nth mappinglist @i))                    ;;map listesinden bir map seçilir.

			(def s2 false)
			(while (and (< @k paragraphcount) (= s2 false))
				(do

					(def j (atom 0))
					(def wrd (list))
					(def wordCount (count (nth paragraph @k)))             ;;paragrafın ilk elemanının uzunluğu

					(while (< @j wordCount)
						(do
							(def tmp (str (nth (nth paragraph @k) @j)))                           ;;paragrafın ilk elemanı.
							(def tmp  (nth tmp 0))
							(def wrd (conj wrd (nth markedlist (c2i tmp))))
							(swap! j inc)))

					(if (= (spell-checker-1 (apply str (reverse wrd))) true)
						(def s2 false)
						(def s2 true)
						)
					(def sntnc (conj sntnc (reverse wrd)))
					(swap! k inc))
				)
			(if (= s2 false)
				(def s1 true))

			(swap! i inc))
		)
	(rest (reverse sntnc))
	)

(defn Gen-Decoder-B-0
	[paragraph]

	(def sntnc (list))
	(def sortedalphabet  '(\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z))
	(def const-sortedalphabet  '(\b \c \d \f \g \h \j \k \l \m \p \q \r \s \u \v \w \x \y \z)) ;; sabit kelimelerden çıkarılmış alfabe..
	(def paragraphcount (count paragraph))                     ;; ((...)(....)) --count.
	(def mostFrequentLetter (mostFrequentSixLetter paragraph))

	(def dLetter (list))
	(def mappinglist (list))
	(def markedlist (list))
	(def mappinglist (permutations const-sortedalphabet))

	(def constSixLetter '(\e \t \a \o \i \n))                  ;;PDF te verilen sabit harfler..
	(def dLetter (into dLetter (set/difference (set sortedalphabet) (set mostFrequentLetter)))) ;; alfabe ile en sık tekrar eden harflerin fark kümesi

	(def const-mapping (zipmap mostFrequentLetter constSixLetter)) ;; 6 harflik mapping..
	;;(println const-mapping)
	;;(def var-mapping (zipmap dLetter const-sortedalphabet))
	;;(println var-mapping)

	(def i (atom 0))
	(def s1 false)
	(def s2 false)

	(while (= s1 false)
		(do

			(def k (atom 0))
			(def markedlist (nth mappinglist @i))                    ;; map listesinden bir map seçilir.
			(def var-mapping (zipmap dLetter markedlist))	           ;; 20 harflik mapping..
			(def all-mapping (merge var-mapping const-mapping))      ;; mapping birleştirme (20+6)

			(def s2 false)
			(while (and (< @k paragraphcount) (= s2 false))
				(do

					(def j (atom 0))
					(def wrd '())
					(def wordCount (count (nth paragraph @k)))             ;;paragrafın ilk elemanının uzunluğu

					(while (< @j wordCount)
						(do

							(def tmp (str (nth (nth paragraph @k) @j)))
							(def tmp (nth tmp 0))                                ;;character....!!
							(def wrd (conj wrd (get all-mapping tmp)))
							(swap! j inc)))

					(if (= (spell-checker-1 (apply str (reverse wrd))) true)
						(def s2 false)
						(def s2 true)
						)
					(def sntnc (conj sntnc (reverse wrd)))
					(swap! k inc))
				)
			(if (= s2 false)
				(def s1 true))

			(swap! i inc))
		)
	(rest (reverse sntnc))

	)

(defn Gen-Decoder-B-1
	[paragraph]
	;you should implement this function
	)

(defn Code-Breaker
	[document decoder]
	(def l (read-as-list document))
	(decoder l)
	)

;; -----------------------------------------------------
;; Test code...

(defn test_on_test_data
	[]
	(let [doc (read-as-list "dictionary1.txt")]
		(println (nth doc 2))
		)
	)


;; test code...
(test_on_test_data)

;;(println (read-as-list "document1.txt"))
;;(println  (fullSentence '((a b c d e) (k l m n p))))
;;(println (sentenceToList "abcde klmnp optry"))
;;(println (mostFrequentSixLetter '((k k x x g z) (l g g g) (m m m) (t t) (w w w w w w)))) ;; 6 farklı harf olmaz ise hata verir.
;;(println (readtwo "dictionary1.txt"))
;;(println (most-frequent-n 26 "abbaxcxbbbxkkkllkkkkklkppppiiiu"))
(println (Code-Breaker "document1.txt" Gen-Decoder-A))
;;(println (Gen-Decoder-A '((y u l u) (y o o m) (y o r o a s t e r))))
;;(println (Gen-Decoder-B-0 '((y o o m) (y o r o a s t e r))))

