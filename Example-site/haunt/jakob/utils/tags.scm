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
;;; Common procedures for tag-based navigation.
;;;
;;; Code:

(define-module (jakob utils tags)
  #:use-module (srfi srfi-1)
  #:export (group-by-tag
            count-tags
            tag-uri))

(define (group-by-tag items accessor)
  "Return lists of the form (tag items) for each tag used in ITEMS. ACCESSOR is
a procedure that takes a single item as an argument and returns its tags."
  (let ((table (make-hash-table)))
    (for-each (lambda (item)
                (let ((tags (accessor item)))
                  (for-each (lambda (tag)
                              (let ((current (hash-ref table tag)))
                                (if current
                                    (hash-set! table tag (cons item current))
                                    (hash-set! table tag (list item)))))
                            tags)))
              items)
    (hash-fold alist-cons '() table)))

(define (count-tags items accessor)
  "Return lists of the form (tag-name count) summarizing tag usage across ENTRIES,
ordered such that tags with greater usage are at the beginning of the list, and
tags with less usage are at the end of the list. ACCESSOR is a procedure that
takes a single item as an argument and returns its tags."
  (sort (map (lambda (tag)
               (list (car tag) (length (cdr tag))))
             (group-by-tag items accessor))
        (lambda (a b) (> (cadr a) (cadr b)))))

(define* (tag-uri prefix tag #:optional (extension ".html"))
  "Return a URI relative to the site's root for a page listing entries in PREFIX
that are tagged with TAG."
  (string-append prefix "/tag/" tag extension))
