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

(define-module (jakob builder blog)
  #:use-module (haunt html)
  #:use-module (haunt page)
  #:use-module (haunt post)
  #:use-module (haunt utils)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (jakob theme)
  #:use-module (jakob utils)
  #:use-module (jakob utils pagination)
  #:use-module (jakob utils sxml)
  #:use-module (jakob utils tags)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (web uri)
  #:export (post-uri
            blog))

;;; Commentary:
;;;
;;; In favor of greater flexibility, Haunt's default 'blog' builder was not used
;;; for this site. This modules implements a similar builder, 'blog', with
;;; pagination and support for tag navigation.
;;;
;;; Code:


;;;
;;; Rendering.
;;;

(define (build-anonymous-comment-url post)
  (let* ((target (format #f "http://jakob.space~a" (post-uri post)))
         (params `(("dontask" . "1")
                   ("me"      . "https://commentpara.de")
                   ("reply"   . ,target)))
         (query (string-join (map (lambda (pair)
                                    (string-concatenate
                                     (list (uri-encode (car pair))
                                           "="
                                           (uri-encode (cdr pair)))))
                                  params)
                             "&")))
    (format #f "https://quill.p3k.io/?~a" query)))

(define (render-article post)
  "Return the SHTML for POST's contents."
  #<(main
     (h1 ,(post-ref post 'title))
     (p ,(date->string (post-date post) "~B ~d, ~Y")
        " ❖ "
        "Tags: "
        ,@(intersperse
           (map (lambda (tag)
                  (hyperlink (tag-uri %tag-prefix tag) tag))
                (post-ref post 'tags))
           ", "))
     ,(when (post-ref post 'crosspost)
        `(p (strong "This is a summary ")
            "of a post that was published elsewhere. "
            "To read the full post, visit "
            ,(hyperlink (post-ref post 'crosspost) "this link")
            "."))
     (article ,(post-sxml post))
     (section
      (@ (id "webmention"))
      (h2 ,(hyperlink "https://indieweb.org/Webmention" "Webmentions")
          " for this Page")
      (ul (@ (id "webmention-container")))
      (form
       (@ (action "https://webmention.io/jakob.space/webmention")
          (method "post"))
       (label "Have you written a "
              ,(hyperlink "https://indieweb.org/responses" "response")
              " to this? Let me know the URL:")
       (input (@ (name "source") (type "url")))
       (input (@ (value "Send Webmention") (type "submit"))))
      (p "Alternatively, you can send an "
         ,(hyperlink (build-anonymous-comment-url post) "anonymous comment")
         ".")
      ,(script "webmention.js"))))

(define (render-preview post)
  "Return the SHTML for a preview of POST."
  (let ((crosspost-uri (post-ref post 'crosspost))
        (local-uri (post-uri post)))
    #<(section
       (h2 ,(hyperlink (or crosspost-uri local-uri) (post-ref post 'title)))
       (p ,(date->string (post-date post) "~B ~d, ~Y")
          ,(when crosspost-uri
             (list " ↻ " (hyperlink local-uri "Crosspost")))
          " ❖ Tags: "
          ,@(intersperse
             (map (lambda (tag)
                    (hyperlink (tag-uri %tag-prefix tag) tag))
                  (post-ref post 'tags))
             ", "))
       (p ,(first-paragraph post))
       ,(hyperlink (or crosspost-uri local-uri) "read more →"))))


;;;
;;; Creation of permalink pages for individual lposts.
;;;

;; Subdirectory for permalink pages.
(define %prefix "/blog")

(define (post-uri post)
  "Return the path of POST relative to the site's root."
  (let* ((file-name (post-file-name post))
         (splice-start (1+ (string-rindex file-name (cut char=? <> #\/))))
         (splice-end (string-rindex file-name (cut char=? <> #\.)))
         (slug (substring file-name splice-start splice-end)))
    (string-append %prefix "/" slug ".html")))

(define (post->page post)
  "Return a Haunt page for POST."
  (make-page (post-uri post)
             (theme #:title (post-ref post 'title)
                    #:description (description-from-post post)
                    #:keywords (post-ref post 'tags)
                    #:content (render-article post))
             sxml->html))


;;;
;;; Navigation based on post tags.
;;;

;; Subdirectory for post listings conditioned on post tags.
(define %tag-prefix "/blog/tag")

(define (tags->pages posts)
  "Return a list of pages for each tag used in POSTS, with said pages containing
only the posts tagged with that tag."
  (flat-map (match-lambda
              ((tag . posts)
               (items->pages render-preview posts
                             (format #f "Posts tagged with \"~a\"" tag)
                             (tag-uri %tag-prefix tag ""))))
            (group-by-tag posts (cut post-ref <> 'tags))))

(define (all-tags posts)
  "Return a page summarizing tag usage across POSTS."
  (define content
    `((h1 "All Tags")
      (ul (@ (id "tag-cloud"))
          ,@(map (match-lambda
                   ((tag count)
                    (hyperlink (tag-uri %tag-prefix tag)
                               `(li ,(format #f "~a (~a)" tag count)))))
                 (count-tags posts (cut post-ref <> 'tags))))))
  (make-page "tag.html"
             (theme #:title "All Tags"
                    #:content content)
             sxml->html))


;;;
;;; Builder.
;;;

(define (blog)
  "Return a Haunt build procedure to create permalinks and post listings for all
of the 'post' objects associated with the site."
  (lambda (site posts)
    (append
     ;; Permalinks.
     (map post->page posts)

     ;; Main post navigation.
     (items->pages render-preview (posts/reverse-chronological posts)
                   "Recent Posts" "index")

     ;; Tag-based navigation.
     (list (all-tags posts))
     (tags->pages posts))))
