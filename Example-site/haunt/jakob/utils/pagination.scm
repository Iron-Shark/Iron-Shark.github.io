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
;;; Common procedures for splitting up large numbers of items across pages.
;;;
;;; Code:

(define-module (jakob utils pagination)
  #:use-module (haunt html)
  #:use-module (haunt page)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (jakob theme)
  #:use-module (jakob utils)
  #:use-module (jakob utils sxml)
  #:export (paginate
            
            render-listing
            items->pages))

(define %items-per-page 10)


;;;
;;; Partitioning.
;;;

(define* (paginate items #:key (items-per-page %items-per-page))
  "Partition ITEMS into list of no more than ITEMS-PER-PAGE items, returning
lists of the form (index, items)."
  (let loop ((index 1)
             (lst items)
             (result '()))
    (if (null? lst)
        result
        (let ((how-many (min %items-per-page (length lst))))
          (loop (1+ index)
                (drop lst how-many)
                (cons (list index (take lst how-many))
                      result))))))


;;;
;;; Rendering.
;;;

(define (render-listing content title previous-page next-page)
  "Return an SHTML document showing CONTENT, with the header TITLE and links to
PREVIOUS-PAGE and NEXT-PAGE."
  #<((h1 ,title)
     ,@content
     (nav
      (@ (id "pagination"))
      ,(when previous-page
         (hyperlink previous-page "← Previous Page"))
      ,(when next-page
         (hyperlink next-page "Next Page →")))))

(define* (items->pages render-item items base-title base-file-name
                       #:key (items-per-page %items-per-page))
  "Return a list of Haunt pages for ITEMS with no more than ITEMS-PER-PAGE items
to a page, with headers containing BASE-TITLE and output file names beginning
with BASE-FILE-NAME. RENDER-ITEM is a procedure returning a SXML rendering of
the item from ITEMS passed as a parameter."
  (define (index->file-name index)
    (if (= index 1)
        (format #f "~a.html" base-file-name)
        (format #f "~a-~a.html" base-file-name index)))
  (map (match-lambda
         ((index subset)
          (let ((title (if (= index 1)
                           base-title
                           (format #f "~a — Page ~a" base-title index)))
                (previous-page (if (>= (1- index) 1)
                                   (index->file-name (1- index))
                                   #f))
                (next-page (if (<= (1+ index) (ceiling/ (length items)
                                                        %items-per-page))
                               (index->file-name (1+ index))
                               #f)))
            (make-page (index->file-name index)
                       (theme #:title title
                              #:content
                              (render-listing (map render-item subset) title
                                              previous-page next-page))
                       sxml->html))))
       (paginate items)))
