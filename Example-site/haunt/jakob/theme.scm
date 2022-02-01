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

(define-module (jakob theme)
  #:use-module (ice-9 match)
  #:use-module (jakob utils sxml)
  #:export (theme))


;;;
;;; SHTML generation in the site's theme.
;;;

(define %stylesheets '("normalize.css" "fonts.css" "highlight.css" "style.css"))
(define %link-rel '(("alternate" "/index.xml" "application/atom+xml")
                    ("icon" "/static/image/favicon.ico" "image/vnd.microsoft.icon")
                    ("me" "https://mastodon.sdf.org/@jakob")
                    ("webmention" "https://webmention.io/jakob.space/webmention")
                    ("pingback" "https://webmention.io/jakob.space/xmlrpc")
                    ("pgpkey authn" "/static/gpg.txt")))
(define %nav-bar-tabs '(("About" "/pages/about.html")
                        ;; ("Projects" "/pages/projects.html")
                        ("Tags" "/tag.html")
                        ("Atom" "/feed.xml")))

(define %title "Jakob's Personal Webpage")

(define %header
  `(header
    ,(hyperlink "/" (image "lambda.svg" "home"))
    (nav (ul
          ,@(map (lambda (tuple)
                   `(li ,(apply hyperlink (reverse tuple))))
                 %nav-bar-tabs)))))

(define %footer
  `(footer
    (div
     (p "© 2015 - 2020 Jakob L. Kreuze")
     ,(image "cc-by-sa-4.0.png"
             "Creative Commons Attribution-ShareAlike 4.0 International (CC
BY-SA 4.0) Logo"))
    (p "Unless otherwise specified, the text and images on this site are free
culture works available under the "
       ,(hyperlink "https://creativecommons.org/licenses/by-sa/4.0/"
                   "Creative Commons Attribution Share-Alike 4.0
International")
       " license.")
    (p "This website is built with "
       ,(hyperlink "http://haunt.dthompson.us/" "Haunt")
       ", a static site generator written in "
       ,(hyperlink "https://gnu.org/software/guile" "Guile Scheme")
       ". The source code is available "
       ,(hyperlink "https://git.sr.ht/~jakob/blog" "here")
       ".")
    (p (a (@ (href "/pages/weblabels.html") (rel "jslicense"))
          "JavaScript license information"))))

(define* (theme #:key
                (title '())
                (description "")
                (keywords '())
                (content '(div "")))
  "Return an SHTML document using the website's theme."
  `((doctype "html")
    (html
     (@ (lang "en"))

     (head
      ,(if (null? title)
           `(title %title)
           `(title ,(string-join (list title %title) " — ")))

      (meta (@ (charset "utf-8")))
      (meta (@ (name "keywords")
               (content ,(string-join keywords ", "))))
      (meta (@ (name "description")
               (content ,description)))
      (meta (@ (name "viewport")
               (content "width=device-width, initial-scale=1.0")))

      ,@(map (lambda (file-name) (stylesheet file-name)) %stylesheets)

      ,@(map (match-lambda
              ((rel href) `(link (@ (rel ,rel) (href ,href))))
              ((rel href type) `(link (@ (rel ,rel) (href ,href) (type ,type)))))
             %link-rel))

     (body
      ,%header
      ,content
      ,%footer))))
