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

;; Place this file in $TEGFS/plugins/share/

(cond-expand
 (guile
  (define-module (tegfs-plugins download-manager)
    :use-module ((euphrates path-extension) :select (path-extension))
    )))

(let ((status (system "command -v exiftool >/dev/null 2>/dev/null")))
  (define code (status:exit-val status))
  (unless (= 0 code)
    (throw 'plugin-initialization-failed "No exiftool program found. Please install it first if you want to use the exif-remover plugin")))

(lambda (root entry fullpath)
  (define preview-prefix (string-append root "/cache"))
  (define preview? (string-prefix? preview-prefix fullpath))
  (and (not preview?)
       (let ((mimetype (cdr (or (assq 'mimetype entry) (cons 'mimetype "unknown")))))
         (string-prefix? "image/" mimetype))
       (let ()
         (define id (cdr (assq 'id entry)))
         (define extension (path-extension fullpath))
         (define path-to-removed
           (string-append root "/tmp/noexif-" id extension))
         (if (file-exists? path-to-removed)
             path-to-removed
             (let ((status (system* "exiftool" "-all=" "-o" path-to-removed fullpath)))
               (define code (status:exit-val status))
               (unless (= 0 code)
                 (throw 'plugin-failed "Exiftool exited with non-zero code. Image sharing must not go through." id))
               path-to-removed)))))
