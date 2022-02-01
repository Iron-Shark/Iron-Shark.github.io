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

(define-module (jakob builder static-pages)
  #:use-module (haunt html)
  #:use-module (haunt page)
  #:use-module (ice-9 ftw)
  #:use-module (jakob builder blog)
  #:use-module (srfi srfi-1)
  #:export (static-pages))

(define* (static-pages)
  (lambda (site posts)
    (define enter? (const #t))

    (define (leaf file-name stat memo)
      (let* ((dest (if (string-suffix? ".sxml" file-name)
                      (string-append (string-drop-right file-name
                                                        (string-length ".sxml"))
                                     ".html")
                      file-name))
             (name (first (string-split (basename dest) #\.)))
             (sxml (primitive-load file-name))
             (contents sxml))
        (cons (make-page dest contents sxml->html) memo)))

    (define (noop file-name stat result)
      result)

    (define (err file-name stat errno result)
      (error "file processing failed with errno: " file-name errno))

    (file-system-fold enter? leaf noop noop noop err '() "pages")))
