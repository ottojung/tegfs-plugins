;;;; Copyright (C) 2023  Otto Jung
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU Affero General Public License as published
;;;; by the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU Affero General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Affero General Public License
;;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Place this file in $TEGFS/plugins/save/

(cond-expand
 (guile
  (define-module (tegfs-plugins download-manager)
    :use-module ((euphrates catch-any) :select (catch-any))
    :use-module ((euphrates directory-files) :select (directory-files))
    :use-module ((euphrates dprintln) :select (dprintln))
    :use-module ((euphrates file-delete) :select (file-delete))
    :use-module ((euphrates read-string-file) :select (read-string-file))
    :use-module ((euphrates string-strip) :select (string-strip))
    :use-module ((euphrates string-to-words) :select (string->words))
    :use-module ((euphrates stringf) :select (stringf))
    :use-module ((euphrates url-get-fragment) :select (url-get-fragment))
    :use-module ((euphrates url-get-hostname-and-port) :select (url-get-hostname-and-port))
    :use-module ((euphrates url-get-path) :select (url-get-path))
    :use-module ((euphrates with-ignore-errors) :select (with-ignore-errors!))
    :use-module ((tegfs a-weblink-q) :select (a-weblink?))
    )))

(define (check-dependency program-name)
  (define command
    (with-output-to-string
      (lambda _
        (display "command -v ")
        (write program-name)
        (display " >/dev/null 2>/dev/null"))))
  (define errormsg
    (with-output-to-string
      (lambda _
        (display "Missing dependency ")
        (write program-name)
        (display ". Please install it first if you want to use the download-manager plugin"))))

  (define status (system command))
  (define code (status:exit-val status))
  (unless (= 0 code)
    (throw 'plugin-initialization-failed errormsg)))

;; Example `target' handled by this function:
;; https://boards.4chan.org/r/thread/18729837#p18729841
(define (download-4chan-media config root current-alist target)
  (define path (url-get-path target))
  (define split (string-split path #\/))
  (define board (list-ref split 1))
  (define thread-id (list-ref split 3))
  (define comment-id/0 (url-get-fragment target))
  (define comment-id (and comment-id/0 (substring comment-id/0 1)))
  (define thread-json-link
    (stringf "https://a.4cdn.org/~a/thread/~a.json" board thread-id))
  (define json-path
    (begin
      (with-ignore-errors! (mkdir root))
      (with-ignore-errors! (mkdir (string-append root "/tmp")))
      (string-append root "/tmp/" "4chan.json")))
  (define _2
    (let ((s (system* "wget" thread-json-link "-O" json-path)))
      (unless (= 0 (status:exit-val s))
        (throw 'download-failed))))
  (define jq-post-path
    (stringf "~a/tmp/4chan.post.jq" root))
  (define jq-select-thread-command
    (if comment-id
        (stringf ".posts[] | select(.no == ~a)" comment-id)
        ".posts[0]"))
  (define _1
    (let ((s (system (stringf "jq ~s ~s > ~s" jq-select-thread-command json-path jq-post-path))))
      (unless (= 0 (status:exit-val s))
        (throw 'jq-failed))))
  (define jq-tmp-path
    (stringf "~a/tmp/4chan.jq" root))
  (define (get-post-field name)
    (let ((s (system (stringf "jq .~a ~s > ~s" name jq-post-path jq-tmp-path))))
      (unless (= 0 (status:exit-val s))
        (throw 'jq-failed)))
    (string-strip (read-string-file jq-tmp-path)))
  (define (unescape escaped)
    (with-input-from-string escaped (lambda _ (read))))
  (define tim (get-post-field "tim"))
  (define filename (unescape (get-post-field "filename")))
  (define ext (unescape (get-post-field "ext")))
  (define note (unescape (get-post-field "com")))
  (define download-link
    (stringf "https://i.4cdn.org/~a/~a~a" board tim ext))

  (define -temporary-file
    (stringf "~a/tmp/4chan-file~a" root ext))

  (let ((s (system* "wget" download-link "-O" -temporary-file)))
    (unless (= 0 (status:exit-val s))
      (throw 'download-failed)))

  `((-temporary-file . ,-temporary-file)
    (target-basename . ,filename)
    (target-extension . ,ext)
    (note . ,note)
    (download? . yes)))

(define (download-youtube-media config root current-alist target)
  (define download?
    (cdr (or (assq 'download? current-alist) (cons #f #f))))
  (define output-dir
    (stringf "~a/tmp/youtubedl" root))
  (define output-template
    (stringf "~a/video" output-dir))

  (and download?
       (begin
         (with-ignore-errors! (mkdir root))
         (with-ignore-errors! (mkdir (string-append root "/tmp")))
         (with-ignore-errors! (mkdir output-dir))

         (for-each file-delete (map car (directory-files output-dir)))

         (let ((s (system* "youtube-dl" target "-o" output-template)))
           (unless (= 0 (status:exit-val s))
             (throw 'download-failed)))

         (let ((-temporary-file (car (map car (directory-files output-dir)))))
           `((-temporary-file . ,-temporary-file)
             (download? . yes))))))

(define (download-booru-media config root current-alist target)
  (define _912
    (begin
      (with-ignore-errors! (mkdir root))
      (with-ignore-errors! (mkdir (string-append root "/tmp")))))

  (define pagepath (string-append root "/tmp/booru.html"))
  (define anspath (string-append root "/tmp/booru-ans"))
  (define -temporary-file (string-append root "/tmp/booru-download"))

  (define _8123
    (let ((s (system* "wget" target "-O" pagepath)))
      (unless (= 0 (status:exit-val s))
        (throw 'download-failed))))

  (define download-link/0
    (let* ((s (system (stringf "pup 'section[data-tags] attr{data-large-file-url}' -f ~s > ~s" pagepath anspath))))
      (string-strip (read-string-file anspath))))

  (define download-link
    (if (not (string-null? download-link/0)) download-link/0
        (let* ((s (system (stringf "pup 'section[data-tags] img attr{src}' -f ~s > ~s" pagepath anspath))))
          (string-strip (read-string-file anspath)))))

  (define _7172
    (let ((s (system* "wget" download-link "-O" -temporary-file)))
      (unless (= 0 (status:exit-val s))
        (throw 'download-failed))))

  (define tags/0
    (let* ((s (system (stringf "pup 'section[data-tags] attr{data-tags}' -f ~s > ~s" pagepath anspath))))
      (string-strip (read-string-file anspath))))

  (define tags
    (if (string-null? tags/0) #f
        (cons 'foreign-tagged=B,$
              (map string->symbol
                   (map (lambda (word) (string-append word "=B")) (string->words tags/0))))))

  (append
   `((-temporary-file . ,-temporary-file)
     (download? . yes))

   (if tags (list (cons 'tags tags)) '())))

(define (handle-by-url config root current-alist target site)
  (or
   (and (equal? "boards.4chan.org" site)
        (download-4chan-media config root current-alist target))
   (and (member site '("youtube.com" "youtu.be" "m.youtube.com" "yewtu.be"))
        (download-youtube-media config root current-alist target))
   (and (string-contains site "booru")
        (download-booru-media config root current-alist target))))

(define (handle config root current-alist target)
  (define site
    (and target (url-get-hostname-and-port target)))
  (catch-any
   (lambda _ (handle-by-url config root current-alist target site))
   (lambda errors
     (parameterize ((current-output-port (current-error-port)))
       (dprintln "Download manager failed to handle a recognized link.")
       (dprintln "  Errors: ~s" errors)
       (newline) (newline)
       #f))))

(define (main config root current-alist)
  (define target
    (cdr (or (assq '-text-content current-alist) (cons #f #f))))
  (define temp
    (cdr (or (assq '-temporary-file current-alist) (cons #f #f))))

  (or
   (and (not temp)
        (string? target)
        (a-weblink? target)
        (handle config root current-alist target))
   '()))

(check-dependency "wget")
(check-dependency "pup")
(check-dependency "jq")
(check-dependency "youtube-dl")

main
