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

;;; Commentary:
;;;
;;; Temporary reader containing my changes to 'html-reader'. This module will
;;; remain until Haunt sees another release and, thus, the fixes are available.
;;;
;;; Code:

(define-module (jakob reader html-prime)
  #:use-module (haunt post)
  #:use-module (haunt reader)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:use-module (sxml simple)
  #:export (html-reader-prime))

(define (read-html-post-prime port)
  (values (read-metadata-headers port)
          (let loop ((ret '()))
            (catch 'parser-error
              (lambda ()
                (match (xml->sxml port)
                  (('*TOP* sxml) (loop (cons sxml ret)))))
              (lambda (key . parameters)
                (reverse ret))))))

(define html-reader-prime
  (make-reader (make-file-extension-matcher "html")
               (cut call-with-input-file <> read-html-post-prime)))
