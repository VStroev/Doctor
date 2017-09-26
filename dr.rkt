#lang scheme/base

(define (pick-random lst)
  (list-ref lst (random (length lst))))

(define (triggers)
  '(((depressed suicide)
    ((when you feel depressed, go out for ice cream)
     (depression is a disease that can be treated)))
   ((mother father parents)
    ((tell me more about your *)
     (why do you feel that way about your *)))
   ((parents friends)
    ((what do you feel about your *)
     (what can you say about *)))))

(define (handlers)
  (list (list (lambda(x) #t) hedge 100.0)
        (list (lambda(x) (not (null? (cadr x)))) remind-phrase  150.0)
        (list (lambda(x) (> (length (car x)) 3)) repeat  200.0)
        (list (lambda(x) (> (length (car x)) 0)) answer-t  300.0)))


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

(define (replace-old pattern replacement lst)
  (cond
    ((null? lst) '())
    ((equal? (car lst) pattern)
     (cons replacement 
           (replace-old pattern replacement (cdr lst))))
    (else
     (cons (car lst)
          (replace-old pattern replacement (cdr lst))))))

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

(define (answer-t responce)
  (let ((trigger-pairs (triggers)))
    (let ((new-pairs  (find-pairs trigger-pairs (car responce))))
          (cond ((null? new-pairs)
                 (hedge responce))
                (else
                 (let ((pair (pick-random new-pairs)))
                   (replace-old '* (pick-random (car pair))  (pick-random (cadr pair)))))))))
  
(define (hedge responce)
  (pick-random
   '((please go on)
     (many people have the same sorts of feelings)
     (many of my patients have told me the same thing)
     (please continue)
     (please tell me more about it)
     (it is okay)
     (oh srsly)
     (wtf))))

(define (repeat user-response)
  (append (qualifier)
          (change-person (car user-response))))

(define (fifty-fifty)
  (= (random 2) 0))

(define (prob n1 n2)
  (< (random n2) n1))

(define (remind-phrase phrases)
  (append '(earlier you said that)
                             (change-person (pick-random (cadr phrases)))))

(define (doctor-driver-loop name phrases)
  (newline)
  (print '**)
  (let
      ((user-response (read)))
    (cond ((equal? user-response '(goodbye))
           (print (list 'goodbye name))
           (print '(see you nextweek))
           (newline))
          (else (let ((response (reply (handlers) user-response phrases)))
                  (print response)
                  (doctor-driver-loop name (append (list user-response) phrases)))))))

(define (reply triplets user-response phrases)
  (let ((handlers (filter (lambda(triplet) ((car triplet) (list user-response phrases))) triplets)))
    ((weight-random handlers) (list user-response phrases))))

(define (weight-random handlers)
  (cond ((null? handlers) '())
        (else
         (print handlers)
         (let ((max-weight (apply max (map caddr handlers))))
           (let ((best-handlers (filter (lambda(x) (equal? (caddr x) max-weight)) handlers)))
             (cadr (pick-random best-handlers)))))))
      
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