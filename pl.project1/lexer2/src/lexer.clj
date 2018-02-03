(ns .lexer)

; *********************************************
; *  341 Programming Languages                *
; *  Fall 2017                                *
; *          Mustafa BİNGÜL                   *
; * 				   141044077                      *
; *********************************************
; *	Annotation :	lexical analyze..						*
; *********************************************
; REGULAR EXPRESSİON KULLANILMAMISTIR,
; ONUN YERİNE KENDİ METHODUMU (DFA) YAZDIM. !!

;;tüm file okunur.
(defn firstReadAllFile
  [file]
  (def allContent)
  (def allContent (slurp file))
  allContent
  )
;;identifier analizi.
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
;;keyword analizi.
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
;;operator analizi.
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
;;dosyanın istenilen formata geçmesi.
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
;;integer analizi.
(defn isIntegerAnalyze
  [intValue]

  (def ci (count intValue))
  (def j (atom 0))
  (def status0 true)
  (def status1 true)

  (if (and (=  (nth intValue 0) \- )  (not= \0 (nth intValue 1)))
    (do
      (while (and (< (+ 1 @j) ci) (= status1 true))
        (do
          (def cntrl (Integer/parseInt (str (nth intValue (+ 1 @j)))))
          (if (integer? cntrl)
            (do)
            (do
              (def status0 false)
              (def status1 false)))

          (swap! j inc)))
      )
    )
  (if (and (=  (nth intValue 0) \- )  (= \0 (nth intValue 1)))
    (do
      (def status0 false))
    )
  (if (= (nth intValue 0) \0 )
    (do
      (def status0 false))
    )

  status0
  )
;;split methodu kullanmıyorum. yazdığım function.
(defn myLexAnalyze
  [conc]

  (def kount (count conc))
  (def i (atom 0))
  (def stringList (list))
  (def newStr "")
  (while (< @i kount)
    (do

      (def concChar (nth conc @i))

      (if (and (not= concChar \tab) (not= (int concChar) 10))
        (do
          (def newStr (str newStr concChar))

          (if (or (= concChar \() (= concChar \)))
            (do
              (def stringList (conj stringList (clojure.string/trim newStr)))
              (def newStr "")
              ))

          (if (= concChar \ )
            (do
              (def stringList (conj stringList (clojure.string/trim newStr)))
              (def newStr "")
              ))
          )
        (do ()))
      (swap! i inc)))

  (def x (atom 0))
  (def control (count stringList))
  (def tokenList (list))
  (while (< @x control)
    (do

      (if (not= (nth stringList @x) "")
        (do
          (def tokenList (conj tokenList (nth stringList @x)))
          ))
      (swap! x inc)))
  tokenList
  )
;;binary analizi.
(defn isBinaryAnalyze
  [binValue]
  (def status false)

  (if (or (= binValue (str "" true)) (= binValue (str "" false)))
    (do (def status true)))

  status
  )
;;token kontrolu
(defn verifyToken
  [analyzeList]

  (def c (count analyzeList))
  (def k (atom 0))
  (def inf)
  (while (< @k c)
    (do

      (def inf "This token is literal or is not identified.")
      (def token (nth analyzeList @k))
      (if (isOperatorAnalyze token)
        (do
          (def inf "This token is an Oparetor.")))
      (if (isKeywordAnalyze token)
        (do
          (def inf "This token is a Keyword.")))
      (if (isIdAnalyze token)
        (do
          (def inf "This token is an Identifier.")))
      (if (isBinaryAnalyze token)
        (do
          (def inf "This token is an BinaryValue.")))

      (printf "%s         ---> %s \n"  token inf)
      (swap! k inc)))
  )
;;lexer..
(defn lexer
  [fileName]

  (def content)
  (def tokens (list))
  (def content (firstReadAllFile fileName))                 ;;read file to content
  (def tokens (myLexAnalyze (newFileContent content)))
  (verifyToken tokens)

  )

(lexer "cff")                                                  ;;FİLE NAME VERİNİZ...

