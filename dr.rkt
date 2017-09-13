#lang scheme/base

(define (pick-random lst)
  (list-ref lst (random (length lst))))

(define (triggers)
  '(((depressed suicide)
    ((when you feel depressed, go out for ice cream)
     (depression is a disease that can be treated)))
   ((mother father parents)
    ((tell me more about your family)
     (why do you feel that way about your parents?)))))

(define (change-person phrase)
  (many-replace '((U you)
                  (R are)
                  (UR your)
                  (are am)
                  (r am)
                  (you i)
                  (u i)
                  (your my)
                  (ur my)
                  (i U)
                  (me U)
                  (am R)
                  (my UR))
                phrase))

(define (ask-patient-name)
  (print '(next!))
  (newline)
  (print '(who are you?))
  (car (read)))

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

(define (answer-trigger trigger answers lst)
  (cond ((null? lst) '())
        (else
         (cond ((member (car lst) trigger)
                (pick-random answers))
               (else (answer-trigger trigger answers (cdr lst)))))))
  
(define (answer-triggers trigger-pairs lst)
  (cond ((null? trigger-pairs) (hedge))
        (else
         (let ((pair (car trigger-pairs)))
           (let ((answer (answer-trigger (car pair) (cadr pair) lst)))
             (cond ((null? answer)
                    ((answer-triggers (cdr trigger-pairs) lst)))
                   (else answer)))))))
  
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

(define (prob n1 n2)
  (< (random n2) n1))

(define (doctor-driver-loop name phrases)
  (newline)
  ;  (princ '**)
  (let
      ((user-response (read)))
    (cond ((equal? user-response '(goodbye))
           (print (list 'goodbye name))
           (print '(see you nextweek))
           (newline))
          (else (let ((response (reply user-response phrases)))
                  (print response)
                  (doctor-driver-loop name (append (list user-response) phrases)))))))

(define (reply user-response phrases)
  (cond ((prob 1 2)
         (cond ((prob 1 2) (answer-triggers (triggers) user-response))
               (else
                (append (qualifier)
                        (change-person user-response)))))
        (else (cond ((or (prob 1 2) (null? phrases))
                     (hedge))
                    (else
                     (append '(earlier you said that)
                             (change-person (pick-random phrases))))))))

(define (session name)
  (print (list 'hello name))
  (print '(what seems to be the trouble?))
  (doctor-driver-loop name '()))

(define (visit-doctor)
  (let ((name (ask-patient-name)))
    (cond ((equal?  name 'kornevgen)
           (print '(gtfo from my office you sick pice of shit)))
          (else (session name)
                (visit-doctor)))))

(visit-doctor)