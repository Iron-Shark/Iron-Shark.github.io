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

(define-module (jakob builder htaccess)
  #:use-module (haunt page)
  #:use-module (ice-9 match)
  #:export (htaccess))

;; Good resource:
;; <https://perishablepress.com/stupid-htaccess-tricks/#ess4>

(define* (htaccess-writer contents #:optional (port (current-output-port)))
  (display (string-join contents "\n") port)
  (newline port))

(define* (htaccess #:key
                   (error-documents '())
                   (redirects '()))
  "Create an .htaccess file at the site's root.

ERROR-DOCUMENTS specifies the file name of the page to display for a specific
HTTP error code: a list of (error-code . file-name) pairs.

REDIRECTS specifies file names to show for certain requests: a list of (pattern
. file-name) pairs."
  (define contents
    `(,@(map (match-lambda
               ((code . file-name)
                (format #f "ErrorDocument ~a ~a" code file-name)))
             error-documents)
      ,@(map (match-lambda
               ((pattern . file-name)
                (format #f "RewriteRule ~a ~a" pattern file-name)))
             redirects)))

  (lambda (site posts)
    (make-page ".htaccess" contents htaccess-writer)))
