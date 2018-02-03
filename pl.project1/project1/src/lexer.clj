(ns .lexer)

(use 'clojure.java.io)
;;(use 'clojure.string)

(defn read-as-list
  "Reads a file containing one word per line and returns a list of words (each word is in turn a list of characters)."
  [filename]
  ; Implement this function...
  (with-open [str (reader filename)]
    (def mlist '( ))
    (doseq [word (line-seq str)]
      (def mlist (concat mlist(apply list (clojure.string/split word #" ")))))
    )
  mlist
  )
(defn firstReadAllFile
  [file]
  (def allContent)
  (def allContent (slurp file))
  allContent
  )

(defn newFileContent
  [fileString]

  (def fileStringCount (count fileString))
  (def i (atom 0))
  (def newContentString "")

  (while (< @i fileStringCount)
    (do

      (if (= (nth fileString @i) \()
        (do (def newContentString (str newContentString "( ")))
        (do ()))
      (if (= (nth fileString @i) \))
        (do (def newContentString (str newContentString " )")))
        (do ()))
      (if (= (nth fileString @i) \tab)
        (do (def newContentString (str newContentString " ")))
        (do ()))
      (if (and (not= (nth fileString @i) \() (not= (nth fileString @i) \)))
        (do (def newContentString (str newContentString (nth fileString @i))))
        (do ()))


      (swap! i inc)))
  newContentString
  )

(defn convertToStringList
  [seqList]
  (def c (count seqList))
  (def stringList '())
  (def x (atom 0))
  (while (< @x c)
    (do
        (if (not= (clojure.string/trim (nth seqList @x)) "")
          (do (def stringList (conj stringList (clojure.string/trim (nth seqList @x)))))
          (do ()))

      (swap! x inc)))
  (reverse stringList)
  )
(defn splitWhiteSpace
  [splitString]

  (def tlist (list))
  (def tlist (concat tlist (apply list (clojure.string/split splitString #" "))))
  tlist
  )

(defn isIdAnalyze [id]

  (def idCount (count id))
  (def x (atom 0))
  (def status true)
  (def wstatus true)
  (while (and (= wstatus true) (< @x idCount))
    (do

      (if (or  (and (<= (int (nth id @x)) (int \Z))
                    (>= (int (nth id @x)) (int \A)))
               (and (<= (int (nth id @x)) (int \z))
                    (>= (int (nth id @x)) (int \a))))
        (do (def status true))
        (do (def status false) (def wstatus false))
        )

      (swap! x inc)))
  status
  )



(defn isKeywordAnalyze
  [iskeyword]

  (def keywords (list))
  (def keywords (list "and","or","not","equal","append","concat","set","deffun","for","while","if","then","else","true","false"))
  (def x (atom 0))
  (def status false)
  (while (< @x 15)
    (do
      (if (= 0 (compare iskeyword (nth keywords @x)))
        (def status true))

      (swap! x inc)))
  status
  )
(defn isOperatorAnalyze
  [isoperator]

  (def operators (list))
  (def operators (list "+","-","/","*","(",")"))
  (def status false)
  (def x (atom 0))
  (while (< @x 6)
    (do
      (if (= 0 (compare isoperator (nth operators @x)))
        (def status true))
      (swap! x inc)))
  status
  )
(defn wrtLN [lst]

  (def x (atom 0))
  (while (< @x (count lst))
    (do
      (println (nth lst @x) (count (nth lst @x)))
      (swap! x inc)))
  )


(defn isLexerAnalyze
  [analyzeList]

  (def i (atom 0))
  (def listCount (count analyzeList))
  (println listCount)
  (def tokenList (list))
  (while (< @i listCount)
    (do
      (def analyzeWord (nth analyzeList @i))
      (def analyzeWordLength (count analyzeWord))

      (println analyzeWordLength analyzeWord)

      (if )


      (swap! i inc)))
  (reverse tokenList)
  )

(def mylist '())
(def MYLIST (list))
(def mylist (read-as-list "cff"))
;(def MYLIST (convertToStringList mylist))


;(def MYLIST (convertToStringList (splitWhiteSpace (newFileContent (firstReadAllFile "cff")))))
;(isLexerAnalyze MYLIST)


(def MYLIST (convertToStringList (splitWhiteSpace (newFileContent (firstReadAllFile "cff")))))

(isLexerAnalyze MYLIST)