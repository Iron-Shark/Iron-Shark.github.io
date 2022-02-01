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

(define-module (jakob builder outbox)
  #:use-module (haunt html)
  #:use-module (haunt page)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (jakob theme)
  #:use-module (jakob utils pagination)
  #:use-module (jakob utils sxml)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:export (outbox))

;;; Commentary:
;;;
;;; My implementation of an "outbox" for sending comments via Webmention [1] to
;;; sites that support it.
;;;
;;; [1]: https://webmention.net/
;;;
;;; Code:

;; Prefix for all pages representing Webmention interactions.
(define %outbox-prefix "/outbox")


;;;
;;; Profile.
;;;

(define %h-card
  `(div (@ (class "u-author h-card"))
        (img (@ (class "u-photo")
                (src "/static/image/profile-picture.jpg")
                (width "40")))
        (a (@ (class "u-url p-name")
              (href "http://jakob.space"))
           "Jakob L. Kreuze")))


;;;
;;; Common rendering code.
;;;

(define (datetime uri date)
  `(p (a (@ (class "u-url") (href ,uri))
         (time (@ (class "dt-published")
                  (datetime ,(date->string date "~4")))
               ,(date->string date "~B ~e, ~Y")))))


;;;
;;; Record type for replies -- by far, my most frequently-used type of
;;; Webmention response.
;;;

(define-record-type <reply>
  (make-reply content date target-uri target-handle)
  reply?
  (content reply-content)
  (date reply-date)
  (target-uri reply-target-uri)
  (target-handle reply-target-handle))

(define (reply-uri reply)
  (let* ((date (date->string (reply-date reply) "~Y-~m-~d-~H:~M:~S"))
         (target (reply-target-handle reply))
         (slug (format #f "reply-~a-~a" target date)))
    (string-append %outbox-prefix "/" slug ".html")))

(define reply
  (match-lambda
    ((target-uri target-handle date-string content)
     (let ((date (string->date date-string "~Y-~m-~dT~H:~M:~S~z")))
       (make-reply content date target-uri target-handle)))))


;;;
;;; Reply rendering.
;;;

(define (render-reply reply)
  (let ((content (cons* (car (reply-content reply))
                        `(@ (class "e-content"))
                        (cdr (reply-content reply)))))
    `(div (@ (class "h-entry"))
          ,%h-card
          (p "In reply to: "
             (a (@ (class "u-in-reply-to")
                   (href ,(reply-target-uri reply)))
                ,(reply-target-handle reply)))
          ,content
          ,(datetime (reply-uri reply) (reply-date reply)))))

(define (render-preview reply)
  (let* ((simple-text? (eqv? 'p (car (reply-content reply))))
         (truncated? (and simple-text?
                          (> (length (cdr (reply-content reply))) 80)))
         (preview (if simple-text?
                      (if truncated?
                          (format #f "~a..."
                                  (substring (cdr (reply-content reply))
                                             0 80))
                          (cdr (reply-content reply)))
                      "[No preview available...]")))
    `(section
      (h2 ,(hyperlink
            (reply-uri reply)
            (format #f "Reply directed towards ~a on ~a"
                    (reply-target-handle reply)
                    (date->string (reply-date reply) "~B ~e, ~Y"))))
      (p ,preview))))

(define (reply->page reply)
  (let ((title (format #f "Reply to ~a" (reply-target-handle reply))))
    (make-page (reply-uri reply)
               (theme #:title title
                      ;; #:description (first-paragraph post)
                      ;; #:keywords (post-ref post 'tags)
                      #:content (render-reply reply))
               sxml->html)))


;;;
;;; Builder.
;;;

(define (outbox)
  (let ((replies (map reply (primitive-load "data/replies.scm"))))
    (lambda (site posts)
      (append
       ;; Permalinks.
       (map reply->page replies)

       ;; Outbox listing.
       (items->pages render-preview
                     (reverse replies)
                     "Webmentions"
                     (string-append %outbox-prefix "/" "index")
                     #:items-per-page 50)))))
