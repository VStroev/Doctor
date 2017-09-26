#lang scheme/base

(define (pick-random lst)
  (list-ref lst (random (length lst))))

(define (triggers)
  '(((depressed suicide)
    ((when you feel depressed, go out for ice cream)
     (when you feel depressed, you shuld eat some tasty food like wokker)
     (depression is a disease that can be treated)))
   ((mother father parents)
    ((tell me more about your *)
     (why do you feel that way about your *)))
   ((parents friends)
    ((what do you feel about your *)
     (what can you say about *)))))

(define (change-person phrase)
  (replace '((are am)
                  (r am)
                  (you i)
                  (u i)
                  (your my)
                  (ur my))
                phrase))

(define (ask-patient-name)
  (print '(next!))
  (newline)
  (print '(who are you?))
  (car (read)))

(define (replace pairs lst)
    (define (findrep pairs word)
      (cond ((null? pairs) word)
            ((equal? (caar pairs) word) (cadar pairs))
            ((equal? (cadar pairs) word) (caar pairs))
            (else (findrep (cdr pairs) word))))
    (if (null? lst) '()
        (cons (findrep pairs (car lst))
              (replace pairs (cdr lst)))))
(define (qualifier)
  (pick-random 
   '((you seem to think)
     (you feel that)
     (why do you believe)
     (why do you say)
     (when did you feel that)
     (can you say why)
     (it is okay that))))

(define (get-triggers triggers lst)
  (cond ((null? triggers) '())
        (else
         (let ((trigger (car triggers)))
               (cond ((member trigger lst)
                      (append (list trigger) (get-triggers (cdr triggers) lst)))
                     (else
                      (get-triggers (cdr triggers) lst)))))))

(define (find-pairs trigger-pairs lst)
  (cond ((null? trigger-pairs) '())
        (else
         (let ((triggers (get-triggers (caar trigger-pairs) lst)))
           (cond ((null? triggers)
                  (find-pairs (cdr trigger-pairs) lst))
                 (else
                  (append (list (list triggers (cadar trigger-pairs))) (find-pairs (cdr trigger-pairs) lst))))))))

(define (answer-t trigger-pairs lst phrases)
  (let ((new-pairs  (find-pairs trigger-pairs lst)))
    (cond ((null? new-pairs)
           (reply lst phrases))
          (else
           (let ((pair (pick-random new-pairs)))
             (replace '* (pick-random (car pair)) (pick-random (cadr pair))))))))
  
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
  (print '**)
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
         (cond ((prob 1 2) (answer-t (triggers) user-response phrases))
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
    (cond ((equal?  name 'krnvgn)
           (print '(gtfo from my office)))
          (else (session name)
                (visit-doctor)))))

(visit-doctor)