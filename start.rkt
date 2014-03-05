;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname start) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

(define los1 (cons "jam" (cons "jelly" (cons "catmosphere" empty))))

;;List-of-string -> List-of-string
;;construct a list of strings that start with cat
(define (s>5 alos) 
  (cond [(empty? alos) empty]
        [else (if (string=? (substring (first alos) 0 3) "cat")
                  (cons (first alos) (s>5 (rest alos)))
                  (s>5 (rest alos)))]))

(define (s<5 alos)
  (cond [(empty? alos) empty]
        [else (if (string=? (substring (first alos) 0 3) "dog")
                  (cons (first alos) (s<5 (rest alos)))
                  (s<5 (rest alos)))]))

(define (s+5 alos ss)
  (cond [(empty? alos) empty]
        [else (if (string=? (substring (first alos) 0 3) ss)
                  (cons (first alos) (s+5 (rest alos) ss))
                  (s+5 (rest alos) ss))]))


(define (oi aloi)
  (cond [(empty? aloi) (empty-scene 300 300)]
        [else (overlay (first aloi)
                       (oi (rest aloi)))]))


(define (f alon)
  (local [(define (g x)
            (< x 5))]
    (filter g alon)))

(define (h alop)
  (local [;;Posn Image -> Image
          ;;draw a circle at the location of the posn
          (define (k x i)
            (place-image (circle 5 'solid 'yellow) (posn-x x) (posn-y x) i))]
    ;;[List-of Posn] [Posn  Image -> Image] -> Image
    (foldr k (rectangle 100 200 'solid 'darkblue) alop)))


(require 2htdp/image)
 
; A Polygon is one of:
; – (list Posn Posn Posn)
; – (cons Posn Polygon)
 
(define MT (empty-scene 50 50))
 
; Polygon -> Image
; add the Polygon p into an image in MT
(define (render-polygon p)
  (local [(define (connect-dots p)
            (cond [(empty? (rest p)) MT]
                  [else (render-line
                         (connect-dots (rest p)) (first p) (second p))]))
          (define (render-line im p q)
            (add-line
             im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))
          (define (last p)
            (cond
              [(empty? (rest (rest (rest p)))) (third p)]
              [else (last (rest p))]))]
    (render-line (connect-dots p) (first p) (last p))))
 
; A NELoP is one of:
; – (cons Posn empty)
; – (cons Posn NELoP)
 
; NELoP -> Image
; connect the Posns in p
(define (connect-dots p)
  (cond
    [(empty? (rest p)) MT]
    [else
      (render-line
        (connect-dots (rest p)) (first p) (second p))]))
 
; Image Posn Posn -> Image
; draw a red line from Posn p to Posn q into im
(define (render-line im p q)
  (add-line
    im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))
 
; NELoP -> Posn
; extract the last Posn from p
(define (last p)
  (cond
    [(empty? (rest (rest (rest p)))) (third p)]
    [else (last (rest p))]))
            
;;end


                       

