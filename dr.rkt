#lang scheme/base

(define (pick-random lst)
  (list-ref lst (random (length lst))))

(define (change-person phrase)
  (many-replace '((are am)
                  (you i)
                  (your my)(i you)
                  (me you)
                  (am are)
                  (my your))
                phrase))

(define (many-replace replacement-pairs lst)
  (cond ((null? replacement-pairs) lst)
        (else (let ((pat-rep (car replacement-pairs)))
                (replace (car pat-rep)
                         (cadr pat-rep)
                         (many-replace (cdr replacement-pairs)
                                       lst))))))

(define (replace pattern replacement lst)
  (cond
    ((null? lst) '())
    ((equal? (car lst) pattern)
     (cons replacement 
           (replace pattern replacement (cdr lst))))
    (else
     (cons (car lst)
           (replace pattern replacement (cdr lst))))))

(define (qualifier)
  (pick-random 
   '((you seem to think)
     (you feel that)
     (why do you believe)
     (why do you say)
     (when did you feel that)
     (can you say why)
     (it is okay that))))

(define (hedge)
  (pick-random
   '((please go on)
     (many people have the same sorts of feelings)
     (many of my patients have told me the same thing)
     (please continue)
     (please tell me more about it)
     (it is okay)
     (oh srsly)
     (wtf))))

(define (fifty-fifty)
  (= (random 2) 0))

(define (doctor-driver-loop name)
  (newline)
  ;  (princ '**)
  (let
      ((user-response (read)))
    (cond ((equal? user-response '(goodbye))
           (print (list 'goodbye name))
           (print '(see you nextweek)))
          (else (print (reply user-response))
                (doctor-driver-loop name)))))
(define (reply user-response)
  (cond ((fifty-fifty) 
         (append (qualifier)
                 (change-person user-response)))
        (else (hedge))))

(define (visit-doctor name)
  (print (list 'hello name))
  (print '(what seems to be the trouble?))
  (doctor-driver-loop name))

(visit-doctor '(Slava))