#lang racket

; Fred Martin fredm@alum.mit.edu
; Day 2 of Advent of Code 2020
; Dec 4 2020
; part one answer so far
; part two done now too

(define in (open-input-file "day2-input.txt"))

; file looks like...

;4-8 g: ggtxgtgbg
;13-14 q: qmzdrtqctvrqsb
;1-5 d: ddddlddzfdd
;1-3 f: pfhff
;4-5 c: ccssnccccc
;etc.

; to get answer:
; (read-and-process 0)

; reads a line of the file and hands it off to be processed,
; recursing to count how many lines pass the test
(define (read-and-process count)
  (let ((str (read-line in)))
    (if (eof-object? str) count
        ; change to "process-iine" for first problem
        (if (process-line2 str) (read-and-process (+ 1 count))
            (read-and-process count)))))

; parses a line into four parts and returns them as a list:
; (min-count max-count char pwd)
(define (parse-line str)
  (let* ((split (string-split str " "))
         (num-range-str (first split))
         (num-range (map string->number (string-split num-range-str "-")))
         (min (first num-range))
         (max (second num-range))
         (char (string-ref (second split) 0))
         (pwd (third split)))
    (list min max char pwd)))

; takes the list-of-four above and
; counts the num of matching chars in the pwd
; and says #t or #f depending whether it fits the min/max range
(define (process-line str)
  (let* ((quad (parse-line str))
         (min (first quad))
         (max (second quad))
         (char (third quad))
         (pwd (fourth quad))
         (numchars (count-chars pwd char)))
    (and (>= numchars min) (<= numchars max))))

; part 2 answer
(define (process-line2 str)
  (let* ((quad (parse-line str))
         (min (first quad))
         (max (second quad))
         (char (third quad))
         (pwd (fourth quad))
         (match (lambda (c) (eq? c char))))
    (xor (match (string-ref pwd (- min 1)))
         (match (string-ref pwd (- max 1))))))
         

; surely there must be a better way?
; counts occurrances of char in str by
; converting str to list of chars;
; mapping eq fcn to change those chars into 1s and 0s
; adding up the 1s.
(define (count-chars str char)
  (let* ((char-list (string->list str))
         (match (lambda (c) (if (eq? c char) 1 0)))
         (char-matches (map match char-list)))
    (foldr + 0 char-matches)))

         
             