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

(define-module (jakob builder blogroll)
  #:use-module (haunt html)
  #:use-module (haunt page)
  #:use-module (haunt utils)
  #:use-module (ice-9 match)
  #:use-module (jakob theme)
  #:use-module (jakob utils)
  #:use-module (jakob utils sxml)
  #:use-module (jakob utils tags)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export (blogroll))

;;; Commentary:
;;;
;;; This module manages pages for listing the blogs that I personally follow and
;;; articles that I enjoyed reading.
;;;
;;; Code:


;;;
;;; Type for entries.
;;;

(define-record-type <entry>
  (make-entry name uri tags comments)
  entry?
  (name     entry-name)
  (uri      entry-uri)
  (tags     entry-tags)
  (comments entry-comments))

(define entry
  (match-lambda
    ((name uri tags) (make-entry name uri tags #f))
    ((name uri tags comments) (make-entry name uri tags comments))))


;;;
;;; Rendering.
;;;

(define* (render-preview name uri tags tag-prefix #:optional comments)
  "Return an SHTML preview of an entry with the given parameters."
  `(section
    ,@(cons*
       `(h2 ,(hyperlink uri name))
       `(p
         ,(intersperse
           (map (lambda (tag)
                  (hyperlink (tag-uri tag-prefix tag) tag))
                tags)
           ", "))
       (or comments '()))))

(define (render-tag-cloud prefix entries)
  "Return SHTML listing the tags of ENTRIES in PREFIX with the number of times
each tag is used."
  `(ul (@ (id "tag-cloud"))
       ,@(map (match-lambda
                ((tag count)
                 (hyperlink (tag-uri prefix tag)
                            `(li ,(format #f "~a (~a)" tag count)))))
              (count-tags entries entry-tags))))

(define* (render-entries title prefix entries #:optional tag)
  "Return an SHTML document listing ENTRIES in PREFIX, with a header of TITLE."
  #<(main
    (h1 ,(if tag
             (format #f "~a - Tagged with \"~a\"" title tag)
             title))
    ,(unless tag (render-tag-cloud prefix entries))
    ,(unless tag `(hr))
    ,@(map (lambda (entry)
             (render-preview (entry-name entry)
                             (entry-uri entry)
                             (entry-tags entry)
                             prefix
                             (entry-comments entry)))
           entries)))

(define (entries->pages title prefix entries)
  "Return a page listing ENTRIES in PREFIX with a header of TITLE, as well as
pages for each of the tags used in ENTRIES."
  (cons
   (make-page (string-append prefix "/index.html")
              (theme #:title title
                     #:content (render-entries title prefix entries))
              sxml->html)
   (map (match-lambda
          ((tag . entries)
           (make-page (tag-uri prefix tag)
                      (theme #:title title
                             #:content (render-entries title prefix entries tag))
                      sxml->html)))
        (group-by-tag entries entry-tags))))



;;;
;;; Builder.
;;;

(define %blogroll
  (list "Blogroll"
        "/blogroll"
        (map entry (primitive-load "data/blogroll.scm"))))

(define %bookmarks
  (list "Bookmarks"
        "/bookmark"
        (map entry (primitive-load "data/bookmarks.scm"))))

(define (blogroll)
  (lambda (site posts)
    (flatten
     (map (cut apply entries->pages <>)
          (list %blogroll %bookmarks)))))
