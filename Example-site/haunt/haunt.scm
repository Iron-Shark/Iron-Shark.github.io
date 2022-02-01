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

(use-modules (haunt builder assets)
             (haunt post)
             (haunt site)
             (jakob builder atom)
             (jakob builder blog)
             (jakob builder blogroll)
             (jakob builder htaccess)
             (jakob builder outbox)
             (jakob builder static-pages)
             (jakob reader html-prime)
             (srfi srfi-19)
             (srfi srfi-26))

;; Replace Haunt's default 'string->date* function with one that recognizes the
;; date format that Org uses.
(define (org-string->date str)
  "Convert STR, a string in Org format, into a SRFI-19 date object."
  (catch 'misc-error
    (lambda () (string->date str "<~Y-~m-~d ~a ~H:~M>"))
    (lambda (key . parameters) (string->date str "<~Y-~m-~d ~a>"))))

(register-metadata-parser! 'date org-string->date)

(site #:title "Jakob's Personal Webpage"
      #:domain "jakob.space"
      #:default-metadata
      '((author . "Jakob L. Kreuze")
        (email  . "zerodaysfordays@sdf.lonestar.org"))
      #:readers (list html-reader-prime)
      #:builders
      (list (atom-feed)
            (blog)
            (blogroll)
            (htaccess #:error-documents '((404 . "pages/404.html")))
            (outbox)
            (static-pages)
            (static-directory "static")))
