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

(define-module (jakob utils sxml)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (hyperlink
            image
            stylesheet
            script

            sanitize-subtree))


;;;
;;; Utility procedures to aid in writing SXML by hand.
;;;

(define (hyperlink target text)
  `(a (@ (href ,target)) ,text))

(define* (image file-name #:optional description)
  (let ((src (string-append "/static/image/" file-name)))
    (if description
        `(img (@ (src ,src) (alt ,description) (title ,description)))
        `(img (@ (src ,src))))))

(define (stylesheet file-name)
  `(link (@ (rel "stylesheet") (href ,(format #f "/static/css/~a" file-name)))))

(define (script file-name)
  (let ((src (string-append "/static/js/" file-name)))
    `(script (@ (src ,src)))))


;;;
;;; A reader extension for implicitly-sanitized SXML trees.
;;;

(define (sanitize-subtree subtree)
  "Remove `nil', `#f', and any unspecified elements from `sbtree'"
  (if (list? subtree)
      (map sanitize-subtree (remove (lambda (elt)
                                      (or (unspecified? elt)
                                          (eq? 'nil elt)
                                          (eq? #f elt)))
                                    subtree))
      subtree))

(define (sxml-reader chr port)
  "Read an SXML literal expression possibly containing unquote forms and
sanitize the resultant subtree."
  `(sanitize-subtree ,(cons 'quasiquote (list (read port)))))

;; Install the reader extension when imported.
(read-hash-extend #\< sxml-reader)
