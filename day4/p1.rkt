#! /opt/homebrew/Caskroom/racket
#lang racket/base

(require expect/rackunit)
(require racket/file)

;; Main function, takes puzzle input, finds matching numbers, sums them all up
;; filename -> number
(define (main filename)
    (define lines (file->lines filename))
    )

;; Iterate through lines
;; listof string number -> number
;; lines, running total -> total
(define (iterate lines (sum 0))
    ;; Convert line into a hashmap with winning nums and a list of given numbers
    (define all_nums (rest (string-split (first lines) ": "))) ;; split
    (set! all_nums (string-split all_nums " | ")) ;; split into two groups
    (define winning_nums (first all_nums))
    (define my_nums (last (all_nums)))
    (set! winning_nums (make-immutable-hash (string->listof_int winning_nums)))
    (set! my_nums (string->listof_int my_nums))

    ;; get      [(hash-ref)]number of matches on card
    (define num_matches get_num_matches winning_nums my_nums)
    (define card_points (expt 2 (- num_matches 1)))
    (iterate (rest lines) (+ card_points sum))
    )

;; Convert string to list of integers
;; string -> listof number
(define (string->listof_int str (intlist empty))
    (define nums (string-split " " str))
    (append_to_listof_nums nums)
    )

;; Helper for string->listof_int
(define (append_to_listof_nums not_converted (converted empty))
    (define firstnum (string->number (first not_converted))) ;; convert first non converted
    (set! converted (cons firstnum converted)) ;; append
    (append_to_listof_nums (rest not_converted) converted)
    )

(check-expect 4 (get_num_matches (make-immutable-hash (list 41 48 83 86 17)) (list 83 86  6 31 17  9 48 53)))

;; Check for matches on cards given sets of numbers
;; hashmap number listof numbers -> numbers
;; hashmap has winning numbers, my_nums is what we're testing for
(define (get_num_matches hashmap my_nums (sum 0))
    (define is_match 0)
    (if (hash-ref hashmap (first my_nums))
        (set! is_match 1)
    )
    (get_num_matches hashmap (rest my_nums) (+ is_match sum))    
)

(check-expect 13 (main "test1"))
