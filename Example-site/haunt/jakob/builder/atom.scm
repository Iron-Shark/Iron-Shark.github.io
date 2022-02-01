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

(define-module (jakob builder atom)
  #:use-module (haunt html)
  #:use-module (haunt page)
  #:use-module (haunt post)
  #:use-module (haunt site)
  #:use-module (haunt utils)
  #:use-module (ice-9 match)
  #:use-module (jakob builder blog)
  #:use-module (jakob utils sxml)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (web uri)
  #:export (atom-feed))

(define* (post->atom-entry site post #:key (blog-prefix ""))
  "Convert POST into an Atom <entry> XML node."
  (let ((uri (or (post-ref post 'crosspost)
                 (post-uri post))))
    `(entry
      (title ,(post-ref post 'title))
      (id ,uri)
      (author
       (name ,(post-ref post 'author))
       ,(let ((email (post-ref post 'email)))
          (if email `(email ,email) '())))
      (updated ,(date->string (post-date post) "~4"))
      (link (@ (href ,uri) (rel "alternate")))
      (summary (@ (type "html"))
               ,(sxml->html-string
                 (append (post-sxml post)
                         (if (post-ref post 'crosspost)
                             `((p "...")
                               (p "This is a crosspost. Click "
                                  ,(hyperlink (post-ref post 'crosspost) "here")
                                  " to read the rest of the article."))
                             '()))))
      ,@(map (lambda (enclosure)
               `(link (@ (rel "enclosure")
                         (title ,(enclosure-title enclosure))
                         (href ,(enclosure-url enclosure))
                         (type ,(enclosure-mime-type enclosure))
                         ,@(map (match-lambda
                                  ((key . value)
                                   (list key value)))
                                (enclosure-extra enclosure)))))
             (post-ref-all post 'enclosure)))))

(define* (atom-feed #:key
                    (file-name "feed.xml")
                    (subtitle "Recent Posts")
                    (filter posts/reverse-chronological)
                    (max-entries 20)
                    (blog-prefix ""))
  "Minor modification to the 'atom-feed' builder in '(haunt builder atom)' to
add support for cross-posts. See the docstring in that manual for details on the
use of this function."
  (lambda (site posts)
    (let ((uri (uri->string
                (build-uri 'http ;; (site-scheme site)
                           #:host (site-domain site)
                           #:path (string-append "/" file-name)))))
      (make-page file-name
                 `(feed (@ (xmlns "http://www.w3.org/2005/Atom"))
                        (title ,(site-title site))
                        (id ,uri)
                        (subtitle ,subtitle)
                        (updated ,(date->string (current-date) "~4"))
                        (link (@ (href ,(string-append (site-domain site)
                                                       "/" file-name))
                                 (rel "self")))
                        (link (@ (href ,(site-domain site))))
                        ,@(map (cut post->atom-entry site <>
                                    #:blog-prefix blog-prefix)
                               (take-up-to max-entries (filter posts))))
                 (@@ (haunt builder atom) sxml->xml*)))))
