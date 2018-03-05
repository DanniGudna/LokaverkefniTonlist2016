;num=einhver tala
;einhver listi
;fallið athugar hvort að talan sé í listanum
(defun isin (num lst)
        		(if
         			(eq (find num lst) num)t nil))


;-----------------------------------------


;num=einhver tala
;bil= seinasta nóta í laglínu
;lst=listi leyfðra tala
;lst2= listi bannaðra tónbila
;lst3= listi talna sem hafa komið áður
;einhver listi
;fallið athugar hvort að talan sé í listanum

(defun isin2 (num bil lst lst2 lst3)
  (let ((number))
    (setq number (mod12 num))
  (if
         			(eq (find number lst) number)(if(neq (find (- num bil) lst2) (- num bil))(if(neq (find number lst3) number)t nil)nil) nil)))

;-----------------------------------------



;lst=einhver listi
;skilar listanum aftur nema búið að taka abs af öllum gildum
(defun abslst (lst)
  (let ((l)(skil)(cnt))
    (setq l (- (list-length lst) 1))
    (setq cnt (list-length lst))
    (loop
      (setq skil (cons(abs (nth l lst)) skil))
      (setq l (- l 1))
      (setq cnt (- cnt 1))
      (when (= cnt 0)(return skil)))))

;-----------------------------------------

;lst=einhver listar
;skilar nýjum lista með mismunin á milli ax og ax-1 og loka talan er b3-a2
(defun tonbillst (a b)
  (let ((skil)(lengd)(cnt))
    (setq lengd (-(list-length a) 1))
    (setq cnt lengd)
    (push (- b(nth lengd a))skil)
    (loop
      (push (- (nth cnt a)(nth (- cnt 1) a)) skil)
      (setq cnt (- cnt 1))
      (when (= cnt 0) (return skil)))))



;-----------------------------------------


;x=einhver tala
;skilar modulo 12 af x
(defun mod12 (x)
    		(let ((y))
    		(setq y 12)
      		(loop
        			(if (> y x) (return x))
        			(setq x (- x y))
        		)
)
)


;==========================================


;max=hágildi
;min=lágildi
;skilar tölu af handahófi á milli min og max,  min<=x<=max  ;;;ath
(defun random2 (max min) (+ (random (- max min)) min))


;=========================================


;macro frá kennara
(defmacro while (test &body body)
  (let ((testlab (gensym))
        (toplab (gensym)))
    `(tagbody
       (go ,testlab)
      ,toplab
      (progn ,@body)
      ,testlab
      (when ,test (go ,toplab)))))


;-------------------------------------------------------




;A=listi
;B=listi
;skorður: a.length=b.length
;skilar fjölda staka sem eru eins og á sama stað í A og B
(defun stadsetningTonbila(A B)
(let ((up))
(setq up 2)

(if (= (nth up A)(nth up B) ) 1 0)))



;--------------------------------------------------


;a:listi
;b:listi
;skorður: a.length = b.length
;finnur stefnu tónbila, ef sama nóta (stefna 0) þá er 0 hlutlaus
;skilar fjolda stefnu sem er eins í tveim jafnlögum listum
(defun stefnatonbila (a b)
  (let ((lsta)(lstb)(listlength))
    (setq lsta a)
    (setq lstb b)
    (setq listlength (-(list-length A) 1))

      (if (>(* (nth listlength lsta)(nth listlength lstb))0) 1 0)))


;--------------------------------------------------

;á að taka abs??
;a=listi
;b=listi
; skorður: listarnir verða að hafa lengd 4
;skilar fjölda staka sem eru eins í a og b
;;;;ath gera hlutlaus að lengd
(defun fjolditonbila (a b)
  (let ((cnt)(lstb)(lsta)(skil))
    (setq cnt 0)
    (setq lsta a)
    (setq skil 0)
    (setq lstb (nth (- (list-length b)1) b))
    (loop
      (if (= lstb (nth cnt lsta))(setq skil (+ skil 1)))
      (setq cnt (+ cnt 1))
      (when (= cnt (list-length a))(return skil)))))


;----------------------------------------------------------------


;a og b eru listar
(defun skyldtonbila (a b)
  (let ((lsta)(lstb)(cnt)(fj)(skil))
    (setq cnt (- (list-length a)1))
    (setq lsta (nth cnt (abslst a)))
    (setq lstb (nth cnt (abslst b)))
    (setq fj (+ lsta lstb))
    (setq skil (/ (- fj 12) 2))
    (if (< 23 fj)0 (if (< fj 13)fj skil))))


;--------------------------------------------------


; a: seinustu 3 nótur í laglínu
; b: 4 gildi sem gætu verið næsta nóta
; c: form sem valið er nótu út frá
;Velur næstu nótu út frá skorðum sem eru settar: stefna,stad og fjoldi
(defun finnanaestunotu (a b c)
  (let ((cnt1)(lstA)(lstB1)(lstB2)(lstB3)(lstB4)(B1)(B2)(B3)(B4)(loka)(lokalst)(stad)(stefna)(fjoldi)(skyld))
    (setq cnt1 3)
    (setq stefna 50)
    (setq stad 65)
    (setq fjoldi 20)
    (setq skyld 3)

    (setq lstB4(tonbillst a (nth 3 b)))
    (setq lstB3(tonbillst a (nth 2 b)))
    (setq lstB2(tonbillst a (nth 1 b)))
    (setq lstB1(tonbillst a (nth 0 b)))


    (push (- (nth 3 c)(nth 2 c)) lstA)
    (push (- (nth 2 c)(nth 1 c))lstA)
    (push (- (nth 1 c)(nth 0 c))lstA)


    ;reiknar vægið á fyrstu nótuna
    (setq B1 (* stad (stadsetningTonbila lstA lstB1)))
    (setq B1 (+ B1 (* stefna (stefnatonbila lstA lstB1))))
    (setq B1 (+ B1 (* fjoldi (fjolditonbila lstA lstB1))))
    (setq B1 (+ B1 (* skyld (skyldtonbila lstA lstB1))))

    ;reiknar vægið á aðra nótuna
    (setq B2 (* stad (stadsetningTonbila lstA lstB2)))
    (setq B2 (+ B2 (* stefna (stefnatonbila lstA lstB2))))
    (setq B2 (+ B2 (* fjoldi (fjolditonbila lstA lstB2))))
    (setq B2 (+ B2 (* skyld (skyldtonbila lstA lstB2))))

    ;reiknar vægið á þriðju nótuna
    (setq B3 (* stad (stadsetningTonbila lstA lstB3)))
    (setq B3 (+ B3 (* stefna (stefnatonbila lstA lstB3))))
    (setq B3 (+ B3 (* fjoldi (fjolditonbila lstA lstB3))))
    (setq B3 (+ B3 (* skyld (skyldtonbila lstA lstB3))))

    ;;reiknar vægið á fjórðu nótuna
    (setq B4 (* stad (stadsetningTonbila lstA lstB4)))
    (setq B4 (+ B4 (* stefna (stefnatonbila lstA lstB4))))
    (setq B4 (+ B4 (* fjoldi (fjolditonbila lstA lstB4))))
    (setq B4 (+ B4 (* skyld (skyldtonbila lstA lstB4))))

    ;lætur öll vægi í list og finnur hæsta vægið af þeim
    (setq loka (max B1 B2 B3 B4))
    (push B4 lokalst)
    (push B3 lokalst)
    (push B2 lokalst)
    (push B1 lokalst)


;skilar því gildi sem var með hæsta vægið
    (loop
      (if (= loka (nth cnt1 lokalst))(return (nth cnt1 b)))
      (setq cnt1 (- cnt1 1)))))



;--------------------------------------------------------------


;;;;cnt2:
;;;;cnt3 fjöldi sameiginlegra tónbila
;a er form sem lagið á að reyna líkjast
;b er laglínan, verður að innihalda amk 3 stök í byrjun
;c er min pitch hæð
;d er mac pitch hæð
;e er skalinn sem þetta er í
;f er bönnuð tóbil
;g er fjöldi nóta sem þú vilt hafa í laglínunni
(defun buatillaglinu (a b c d e f g)
  (let ((lstrandom)(lstset)(lstskil)(x))
    ;(setq cnt 0)
    (setq lstskil (reverse b))
    (loop
      (setq lstset '())
      ;; velur 4 random stök sem gætu komið næst
      (setq lstrandom (tonlist2 d c 4 e f (nth 0 lstskil)))
      (setq lstset (cons (nth 2 lstskil) lstset))
      (setq lstset (cons (nth 1 lstskil) lstset))
      (setq lstset (cons (nth 0 lstskil) lstset))
      (setq lstset (reverse lstset))

      (setq x (finnanaestunotu lstset lstrandom a))
      (setq lstskil ( cons x lstskil))

      (if (>= (list-length lstskil) g)(return (reverse lstskil))))))



;---------------------------------------

;max=hágildi
;min=lággildi
;fj=tala, hversu mörg stök í listanum á að skila
;lst=listi, skali sem öll gildin þurfa að vera inn í
;lst2= listi bannaðara tónbila
;bil= seinasta talan í lokalista
(defun tonlist2 (max min fj lst lst2 bil)
       (let ((x)(y)(z))
         (setq y '())
         (loop
           ;býr til random tölu á bilinu min<x<max
           (setq x (random2 max min))
           ;athugar hvort að random talan sé í listanum, ef já þá bætt við lista y og fj minnkað um einn
           ;annars fundið aðra random tölu
           (if (eq(isin2 x bil lst lst2 z)t)(progn(setq y (cons x y))(setq z (cons x z))))
           ;þegar listinn y er með fj stök þá skilað y
           (when (= fj (list-length y)) (return y)))))



;------------------------------




;a=listi
;b=tala
;skilar hlutfallið a[i]/b af öllum stökum í a
(defun durationofattack (a b)
  (let ((prosent)(durationlist)(cnt))
    (setq prosent b)
    (setq durationlist '())
    (setq cnt (list-length a))
    (setq cnt (- cnt 1))
    (loop
      (push (/ (* prosent (nth cnt a))100) durationlist)
      (setq cnt (- cnt 1))
      (when (< cnt 0) (return durationlist)))))

;a=listi
;b=tala min
;c=tala max
;skilar hlutfallið a[i]/b af öllum stökum í a
(defun durationofattack2 (a b c)
  (let ((prosent)(durationlist)(cnt))
    ;(setq prosent b)
    (setq durationlist '())
    (setq cnt (list-length a))
    (setq cnt (- cnt 1))
    (loop
      (setq prosent (random2 c b))
      (push (/ (* prosent (nth cnt a))100) durationlist)
      (setq cnt (- cnt 1))
      (when (< cnt 0) (return durationlist)))))



;------------------------------------------------------


;max=hágildi
;min=lággildi
;fj=tala, fjöldi staka sem á að vera í listanum sem er skilað
;skilar fj staka lista af gildum á bilinu min-max
(defun velocitylist (max min fj)
       (let ((x)(y))
         (setq y '())
         (loop
           (setq x (random2 max min))
           (setq y (cons x y))
           (setq fj (- fj 1))
           (when (= fj 0) (return y)))))


;----------------------------------


;x er fast gildi sem er gurnn lengd nótu
;y1 y2 y3 segir til um númer hvers stakas á að breytast
;lst inniheldur gildi sem gætu breyst í
(defun makeduration3 ( x y1 y2 y3 lst fj)
  (let ((skil))
    (setq skil (cons x skil))
    (loop
      (if (= (mod (+(list-length skil)1) y1)0)(setq skil (cons (nth 0 lst) skil))
          (if (= (mod (+(list-length skil)1) y2)0)(setq skil (cons (nth 1 lst) skil))
              (if (= (mod (+(list-length skil)1) y3)0)(setq skil (cons (nth 2 lst) skil))
                  (setq skil (cons x skil)))))
      (when (=(list-length skil) fj)(return (reverse skil))))))

;--------------------------------------

;;;Notað til þess að lengd nótna passi alltaf, attack þarf að vera duration gildi seiansta staks til að það gangi rétt
(defun makeattackfromduration (duration)
  (let ((last)(skil)(cnt))
    (setq last (nth (-(list-length duration)1) duration))
    (print last)
    (setq cnt (-(list-length duration)2))
    (loop
      (print cnt)
      (setq skil (cons (nth cnt duration) skil))
      (print skil)
      (setq cnt (- cnt 1))
      (when (< cnt 0)(return (setq skil (cons last skil)))))))

;x er fast gildi sem er gurnn lengd nótu
;y1 y2 y3 segir til um númer hvers stakas á að breytast
;lst inniheldur gildi sem gætu breyst í
(defun makeduration2 ( x y1 y2 lst fj)
  (let ((skil))
    (setq skil (cons x skil))
    (loop
      (if (= (mod (+(list-length skil)1) y1)0)(setq skil (cons (nth 0 lst) skil))
          (if (= (mod (+(list-length skil)1) y2)0)(setq skil (cons (nth 1 lst) skil))
                  (setq skil (cons x skil))))
      (when (=(list-length skil) fj)(return (reverse skil))))));;


;x er fast gildi sem er gurnn lengd nótu
;y1 y2 y3 segir til um númer hvers stakas á að breytast
;lst inniheldur gildi sem gætu breyst í
(defun makeduration1 ( x y1 lst fj)
  (let ((skil))
    (setq skil (cons x skil))
    (loop
      (if (= (mod (+(list-length skil)1) y1)0)(setq skil (cons (nth 0 lst) skil))
                  (setq skil (cons x skil)))
      (when (=(list-length skil) fj)(return (reverse skil))))));;

;kennarastuff
(defun makerythmlist (x y)
(make-list x :initial-element y))

(defun makechannellist (x y)
(make-list x :initial-element y))

(defun makeattacklist (x y)
(make-list x :initial-element y))


;----------------------------------------------

;;;kennara midi stuff:

 (defvar pitch-list '( 60 60 60 60))

 (defvar duration-list '( 60 60 60 60))

 (defvar velocity-list '( 60 60 60 60))

 (defvar attack-list '( 60 60 60 60))

 (defvar CHANNELS-LIST '( 0 0 0 0))

(defun time-list2 (lst)
               (reverse (maplist #'(lambda  (el)
                                           (apply #'+  el))
                                  (reverse lst))))

(defvar *time-sign* nil)
(defvar *numb* 4)
(defvar *mesure* 4)

(Defun set-time-sign (num mes)
(setf *numb* num)
(setf *mesure* mes))

(defun time-list (lst)
(cond((< 0 (length lst))
          (let (( ll (if (= (car lst) 0) lst(cons 0 lst))))
               (reverse (maplist #'(lambda  (el)
                                           (apply #'+  el))
                                  (reverse ll)))))))
(defun No-parenth (lst)
(let ((result1))
  (while lst
                         (cond (( numberp (car lst)) (push  (pop lst)result1))
                               ((listp (car lst))  (mapcar #'(lambda (ll)
                                                         (push   ll result1)) (pop lst)))))
(reverse result1)))



(defun round2 (x)
    (if (numberp x)
        (if (minusp x)
            (* (truncate (+  (abs x) 0.5)) -1)
            (truncate (+ x 0.5))) x))


(defun variable-length-midi-time (ticks)
  (let ((high-byte1 0)(high-byte2 0)(high-byte3 0)
        (mod-ticks (mod ticks #x80)))
    (cond
      ((> ticks #x1FFFFF)
        (setq high-byte3 (+ #x80 (truncate (/ ticks #x200000))))
        (setq high-byte2 (+ #x80 mod-ticks))
        (setq high-byte1 (+ #x80 mod-ticks))
        (setq ticks mod-ticks)
        (list high-byte3 high-byte2 high-byte1 ticks))
      ((> ticks #x3FFF)
        (setq high-byte2 (+ #x80 (truncate (/ ticks #x4000))))
        (setq high-byte1 (+ #x80 mod-ticks))
        (setq ticks mod-ticks)
        (list high-byte2 high-byte1 ticks))
      ((> ticks #x7F)
        (setq high-byte1 (+ #x80 (truncate (/ ticks #x80))))
        (setq ticks mod-ticks)
        (list high-byte1 ticks))
      (t (list ticks)))
   ))



(defun covert-length-to-4-byte-list (len)
  (let ((byte1 0)(byte2 0)(byte3 0)(byte4 0))
    (cond
       ((> len #xFFFFFF)
         (setq byte4 (truncate (/ len #x1000000)))
         (setq byte3 (mod len #x1000000))
         (setq byte3 (mod len #x10000))
         (setq byte2 (mod len #x10000))
         (setq byte1 (mod len #x100)))
       ((> len #xFFFF)
         (setq byte3 (truncate (/ len #x10000)))
         (setq byte2 (mod len #x10000))
         (setq byte1 (mod len #x100)))
       ((> len #xFF)
         (setq byte2 (truncate (/ len #x100)))
         (setq byte1 (mod len #x100)))
       (t  (setq byte1 len)))
    (list byte4 byte3 byte2 byte1)))


;(defmacro while (test &body body)
;  (let ((testlab (gensym))
 ;       (toplab (gensym)))
  ;  `(tagbody
   ;    (go ,testlab)
    ;  ,toplab
     ; (progn ,@body)
      ;,testlab
      ;(when ,test (go ,toplab)))))

;============================================================

(defun  make-variable-length-midi-delta-times  (midi-list)
  (let ((res)(time-now 0))
    (WHILE midi-list
      (push (append (variable-length-midi-time (- (caar midi-list) time-now))
                    (cdar midi-list)) res)
      (setq time-now (caar midi-list))
      (pop midi-list))
    (no-parenth (nreverse res) )))


(defun two-incr-lists (notes  amp pitch)
(let ((out-list)
      (new-amp   (mapcar #'(lambda (l1)
                           (list (nth 0 l1)(+ 176 (nth 1 l1))
                                 (nth 2 l1)(nth 3 l1)))(copy-list amp)))
      (new-pitch   (mapcar #'(lambda (l1)
                           (list (nth 0 l1)(+ 224 (nth 1 l1))
                                 (nth 2 l1)(nth 3 l1)))(copy-list pitch))))
 (while (or  new-amp notes   new-pitch)
;(if   (eq (length notes)0)(progn(print-EVE (length new-amp))(print-EVE (length new-pitch))))
(cond ((and (neq nil notes)
            (or(eq  nil new-amp)
               (<= (car (car notes))(car(car new-amp))))
            (or(eq  nil new-pitch)
               (<= (car (car notes))(car(car new-pitch)))))
       (push (pop notes)out-list))
      ((and (neq nil new-amp)
            (or(eq  nil notes)
               (> (car (car notes))(car(car new-amp))))
            (or(eq  nil new-pitch)
               (> (car(car new-pitch))(car(car new-amp)))))
       (push (pop new-amp) out-list))
      (t   (if (neq nil new-pitch)(push (pop new-pitch)out-list)))
))
(reverse out-list)))

(defvar AMPLITUDE-LIST nil)
(defvar PITCH-ENVELOPE-LIST nil)
(defvar look-midi nil)

(defun  make-midi-file-list  (attacks pitches durs vels chans)

 (setq chans (mapcar #'(lambda (l1)(+ 1 l1))   (copy-list chans)))
  (let ((t-time)(note)(midi-list))
    (while attacks
      (setq t-time (pop attacks))
      (setq note (pop pitches))
      (push (list
                t-time
                (+ #x8f (car chans))
                 note
                (pop vels))
              midi-list)
      (push (list
                (+ t-time (car durs))
                (+ #x8f (car chans))
                note
                 0)
            midi-list)
         (pop durs)
         (pop chans));(print-EVE  midi-list)

(if(or (neq nil AMPLITUDE-LIST)(neq nil PITCH-ENVELOPE-LIST))
    (setq  midi-list (two-incr-lists(sort
       (nreverse midi-list) #'< :key #'(lambda (a) (car a)))
  AMPLITUDE-LIST PITCH-ENVELOPE-LIST))
         ;  (setq midi-list  (nreverse midi-list) ))
   (setq  midi-list (sort  (nreverse midi-list) #'< :key #'(lambda (a) (car a)))))
;;;
(setq look-midi   midi-list)
      (make-variable-length-midi-delta-times     midi-list  )))

(defun make-midi-file-0 (attacks pitches durs vels chans )
  (let ((data (make-midi-file-list attacks pitches durs vels chans))
        (track-info
          '(#x00 #xff #x58 #x04 #x04  #x02 #x24 #x08
            #x00 #xff #x51 #x03 #x07 #xa1 #x20))
        (track-end '(#x00 #xff #x2f #x00)))

    (append
      (no-parenth(list #x4D #x54 #x68 #x64
       #x00 #x00 #x00 #x06
       #x00 #x00
       #x00 #x01
       #x00 #x60

     #x4D #x54 #x72 #x6B
    ;  #x00 #xff #x04 #x0E
    ; #x00 #x00 #x0 #x3B
     ; #x00 #x00 #x0 #x24

)



)
      (covert-length-to-4-byte-list (+ (length track-info)(length data)(length track-end)))
      track-info
      data
      track-end)))





;======================================================

 (defun set-mac-file-type (path mac-file-type)
 (declare (ignore path mac-file-type)) t)

 (defun PW-midi-file-SAVE3  ( name1 path  )

 (let ((new-att (time-list(append (list 0)  (cdr  (mapcar #'(lambda (l1 )(round2 (* 2 l1 )))  attack-list) ))))
    (new-dur   (mapcar #'(lambda (l1 )(round2 (* 2 l1 )))    duration-list) ))

 (PW-midi-file-SAVE1  (make-midi-file-0
 new-att pitch-list new-dur velocity-list channels-list)  name1 path )))

 (defun PW-midi-file-SAVE1  (midi-data-list  name1 path  )
  (let* ((new-file))

 (setq *PRINT-BASE* 2)

 (setq new-file (ccl::create-file  (make-pathname :directory path :name   name1  ;:type "MID"
 ) ))

     (WITH-OPEN-FILE  (out new-file :direction :output :IF-EXISTS :overwrite
                         :element-type '(unsigned-byte  8)
 )
        (WHILE midi-data-list
        (write-byte       (pop midi-data-list)      out  )))
 (setq *PRINT-BASE* 10)
;(set-mac-file-type new-file "Midi")
     ))

;=======================================================




(defun position-obj-char (obj lst)
  (let ((pos)(count  0)
	(lst1 (copy-list lst))
	(el 0)
	(test )
	(max1 0))
    (WHILE lst1 (setq el (pop lst1))
(cond ((stringp el)
             (setq test (CCL::STRING-LESSP  el obj))
  (if    (and (neq nil test)(<=  max1 test)) (progn (setq max1 test)(setq pos count) ))))(incf count))
        pos))


;----------------------------------------------
;----------------------------
;;;;FINAL PART;;;;;


;;ATH KEYRA ÞETTA ÞEGAR BÚIÐ ER AÐ DEFINE ÖLL FUNCTIONS ANNARS MUN ÞETTA EKKI VIRKA
;;;fara eftir númera röð

(PW-midi-file-SAVE3 "NYTT.mid" "/Users/Danni/Desktop/")


(setq attack-list (makeattackfromduration duration-list)) ;stillir ekkert bil milli nótna  ;;4

;;;nota annaðhvort eftir hitt fyrir ofan
(setq attack-list (durationofattack attack-list 110)) ; stillir smá bil allstaðar?  ;;5
(setq attack-list (durationofattack2 attack-list 80 125)) ;;5


(setq pitch-list (buatillaglinu '(60 72 68 84) '(60 72 68 84) 60 84 '(0 2 4 5 7 9 11) '(1 -1 5 -5) 100)) ;;6

(setq duration-list (makeduration2 24 20 4 '(198 96) 100)) ;;3

(setq velocity-list (velocitylist 150 100 100)) ;;2

(setq channels-list (makechannellist 100 11))  ;;1
