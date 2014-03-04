#lang racket

; include OSC
(require racket/udp
         inf/opensoundcontrol/osc-to-bytes
         inf/opensoundcontrol/osc-defns
         inf/opensoundcontrol/bytes-to-osc)

(define host-ip "127.0.0.1") ; IP address of the receiver
(define host-port 12345) ; port number on the receiver

(define the-socket (udp-open-socket))

; Sending OSC messages
; send-command takes a message name and a list of arguments
;  to send to the host/port given above
(define (send-command message-name args)
  (udp-send-to the-socket host-ip host-port 
     (osc-element->bytes
        (osc-message (string->bytes/utf-8 message-name) args))))

; ============
; genereert losse getallen van de Fibonacci-reeks
; (fib 5) ==> 5
(define (fib n)
  (fib-iter 1 0 n))
 
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

; genereert lijsten van getallen van de Fibonacci-reeks
; (fibonacci 10) ==> '(1 1 2 3 5 8 13 21 34 55) 
(define (fibonacci n (count 0) (n-1 0) (n-2 0)) 
(if (<= n 0) '() 
(let ((next (if (< count 2) 
1 
(+ n-1 n-2)))) 
(cons next (fibonacci (- n 1) (+ count 1) n-2 next)))))

; =====================
; =====================

; lees lijst uit, met pauzes van x sec
(define (leesuitlijst lst)
  (if (empty? lst) (void) ;if lege lijst, stop.
      (begin
        (displayln (first lst)) ;laat eerste uit lijst zien
        (send-command "/waarde" (list (+ (first lst) 60) (* (first lst) 110) (+ 48 (length lst)))) ;stuurt deze via OSC
        (sleep (* (first lst) 0.1)) ; tijd er tussen
        (leesuitlijst (rest lst))))) ;rest gaat weer in de leesuitlijst

(leesuitlijst (fibonacci 10))