;;; Copyright © 2019 - 2020 Jakob L. Kreuze <zerodaysfordays@sdf.org>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program. If not, see
;;; <http://www.gnu.org/licenses/>.

(define-module (jakob utils)
  #:use-module (haunt post)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (maybe-cons*
            maybe-list
            date->string*
            intersperse
            first-paragraph
            description-from-post))

(define (maybe-list . args)
  "Create a list of all ARGS that are neither #f nor unspecified."
  (remove (lambda (element)
            (or (not element) (unspecified? element)))
          args))

(define (maybe-cons* . args)
  "Cons all ARGS that are neither #f nor unspecified."
  (apply cons* (apply maybe-list args)))

(define (date->string* date)
  "Convert DATE to human readable string."
  (date->string date "~a ~d ~B ~Y"))

(define (intersperse lst delim)
  "Return the elements of LST delimited by DELIM, such that the resultant list
is of an odd length and every second element is DELIM."
  (if (<= (length lst) 1)
      lst
      (cons* (car lst)
             delim
             (intersperse (cdr lst) delim))))

(define (remove-footnote-references content)
  "Remove any <sup> elements from CONTENT."
  (map (lambda (elt)
         (if (list? elt)
             (remove-footnote-references elt)
             elt))
       (remove (lambda (elt)
                 (and (list? elt) (eq? 'sup (car elt))))
               content)))

(define (first-paragraph post)
  (let loop ((sxml (post-sxml post))
             (result '()))
    (match sxml
      (() (remove-footnote-references (reverse result)))
      ((or (('p ...) _ ...) (paragraph _ ...))
       (remove-footnote-references (reverse (cons paragraph result))))
      ((head . tail)
       (loop tail (cons head result))))))

(define (description-from-post post)
  (define (first-elem sxml)
    (if (and (list? sxml) (positive? (length sxml)))
        (if (symbol? (first sxml))
            sxml
            (let ((reduced (remove null? (map first-elem sxml))))
              (if (positive? (length reduced))
                  (first reduced)
                  '())))
        '()))
  (define (collect-strings elt res)
    (cond ((null? elt) res)
          ((string? (car elt)) (collect-strings (cdr elt) (cons (car elt) res)))
          ((list? (car elt)) (if (and (positive? (length (car elt)))
                                      (not (eq? '@ (caar elt))))
                                 (let ((nested (collect-strings (car elt) (list))))
                                   (collect-strings (cdr elt) (append nested res)))
                                 (collect-strings (cdr elt) res)))
          (else (collect-strings (cdr elt) res))))
  (let* ((sxml (first-paragraph post))
         (extracted (collect-strings (first-elem sxml) (list))))
    (string-join (map string-trim-both (reverse extracted)) " ")))
