;;; helm-git-files.el --- helm for git files

;; Author: INA Lintaro <tarao.gnn at gmail.com>
;; Version: 0.1
;; Keywords: helm, git

;; This file is NOT part of GNU Emacs.

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(eval-when-compile (require 'cl))
(require 'vc-git)
(require 'helm-config)
(require 'sha1 nil t)

(defgroup helm-git-files nil
  "helm for git files."
  :prefix "helm-git-files:" :group 'helm)

(defcustom helm-git-files:update-submodules-once nil
  "t means to update file list in submodules only once."
  :type 'boolean
  :group 'helm-git-files)

(defconst helm-git-files:ls-args
  '((modified . ("--modified"))
    (untracked . ("--others" "--exclude-standard"))
    (all . nil)))

(defconst helm-git-files:update-check-functions
  '((modified . helm-git-files:status-updated-p)
    (untracked . helm-git-files:status-updated-p)
    (all . helm-git-files:head-updated-p)))

(defconst helm-git-files:status-expire 1)

(defsubst helm-git-files:chomp (str)
  (replace-regexp-in-string "[\r\n]+$" "" str))

(defun helm-git-files:hash (obj)
  obj)
(cond ((fboundp 'secure-hash)
       (defun helm-git-files:hash (obj)
         (secure-hash 'sha1 obj)))
      ((fboundp 'sha1) (fset 'helm-git-files:hash 'sha1)))

(defun helm-git-files:command-to-string (&rest args)
  (with-output-to-string
    (with-current-buffer standard-output
      (apply 'vc-git-command (current-buffer) 0 nil args))))

(defun helm-git-files:root-1 ()
  (file-name-as-directory
   (helm-git-files:chomp
    (helm-git-files:command-to-string "rev-parse" "--show-toplevel"))))

(defun helm-git-files:root ()
  (or (vc-file-getprop default-directory 'git-root)
      (vc-file-setprop default-directory 'git-root
                       (helm-git-files:root-1))))

(defun helm-git-files:head (&optional root)
  (let ((default-directory (or root default-directory)))
    (helm-git-files:chomp
     (helm-git-files:command-to-string "rev-parse" "HEAD"))))

(defun helm-git-files:ls (buffer &rest args)
  (apply 'vc-git-command buffer 0 nil "ls-files" args))

(defun helm-git-files:status-1 ()
  (helm-git-files:command-to-string "status" "--porcelain"))

(defun helm-git-files:status-hash (&optional root)
  "Get hash value of \"git status\" for ROOT repository.
The status and its hash value will be reused until
`helm-git-files:status-expire' seconds after the last time
they have been updated."
  (let* ((default-directory (or root default-directory))
         (prop 'helm-git-files:status-hash)
         (info (vc-file-getprop default-directory prop))
         (last (plist-get info :last))
         (now (float-time)))
    (when (or (not (numberp last))
              (> now (+ helm-git-files:status-expire last)))
      (let ((hash (helm-git-files:hash (helm-git-files:status-1))))
        (setq info (plist-put info :hash hash))))
    (setq info (plist-put info :last now))
    (vc-file-setprop default-directory prop info)
    (plist-get info :hash)))

(defun helm-git-files:once-updated-p (root &optional key)
  (let* ((key (or (and key (format "-%s" key)) ""))
         (prop (intern (format "helm-git-files:once-updated%s" key)))
         (updated (vc-file-getprop root prop)))
    (unless updated
      (vc-file-setprop root prop t)
      t)))

(defun helm-git-files:head-updated-p (root &optional key)
  (let* ((key (or (and key (format "-%s" key)) ""))
         (prop (intern (format "helm-git-files:last-head%s" key)))
         (last-head (vc-file-getprop root prop))
         (head (helm-git-files:head root)))
    (unless (and last-head (string= head last-head))
      (vc-file-setprop root prop head)
      t)))

(defun helm-git-files:status-updated-p (root &optional key)
  (let* ((key (or (and key (format "-%s" key)) ""))
         (prop (intern (format "helm-git-files:last-status%s" key)))
         (last-status (vc-file-getprop root prop))
         (status (helm-git-files:status-hash root)))
    (unless (and last-status (string= status last-status))
      (vc-file-setprop root prop status)
      t)))

(defun helm-git-files:updated-p (mode root &optional key update-once)
  "Check if the status hash value for ROOT repository is updated.
MODE specifies how to check the update status.  The update status
is tracked for each KEY separately."
  (let ((funs (or (and update-once 'helm-git-files:once-updated-p)
                  (cdr (assq mode helm-git-files:update-check-functions))
                  '(helm-git-files:head-updated-p
                    helm-git-files:status-updated-p))))
    (unless (listp funs) (setq funs (list funs)))
    (loop for fun in funs
          always (funcall fun root key))))

(defun helm-git-files:init-fun (what &optional root update-once)
  `(lambda ()
     (let* ((root (or ,root (helm-git-files:root)))
            (buffer-name (format " *helm candidates:%s:%s*" root ',what))
            (buffer (get-buffer-create buffer-name)))
       (helm-attrset 'default-directory root) ; saved for `display-to-real'
       (helm-candidate-buffer buffer)
       (when (helm-git-files:updated-p ',what root ',what ,update-once)
         (let ((default-directory root)
               (args (cdr (assq ',what helm-git-files:ls-args))))
           (apply 'helm-git-files:ls buffer "--full-name" args))))))

(defun helm-git-files:display-to-real (name)
  (expand-file-name name (helm-attr 'default-directory)))

(defun helm-git-files:source (what &optional root repository update-once)
  (let ((name (concat (format "Git %s" (capitalize (format "%s" what)))
                      (or (and repository (format " in %s" repository)) ""))))
    `((name . ,name)
      (init . ,(helm-git-files:init-fun what root update-once))
      (candidates-in-buffer)
      (delayed)
      (type . file)
      (display-to-real . helm-git-files:display-to-real))))

(defun helm-git-files:submodules-by-dot (&optional dotgitmodule)
  (let ((exp "^[[:space:]]*path[[:space:]]*=[[:space:]]*\\(.*\\)[[:space:]]*$")
        (result (list)))
    (with-temp-buffer
      (insert-file-contents-literally dotgitmodule)
      (goto-char (point-min))
      (while (re-search-forward exp nil t)
        (push (match-string 1) result))
      (reverse result))))

(defun helm-git-files:submodules-by-foreach (&optional root)
  (let ((default-directory root)
        (args '("submodule" "--quiet" "foreach" "echo $path")))
    (loop for module in (split-string
                         (helm-git-files:chomp
                          (apply 'helm-git-files:command-to-string args))
                         "[\r\n]+")
          if (> (length module) 0)
          collect module)))

(defun helm-git-files:submodules (&optional root)
  (let* ((root (or root (helm-git-files:root)))
         (dotgitmodule (expand-file-name ".gitmodules" root)))
    (if (file-exists-p dotgitmodule)
        (helm-git-files:submodules-by-dot dotgitmodule)
      (helm-git-files:submodules-by-foreach root))))

;;;###autoload
(defun helm-git-files:git-p (&optional root)
  (ignore-errors (helm-git-files:status-hash root)))

;;;###autoload
(defvar helm-git-files:modified-source nil)
(setq helm-git-files:modified-source
      (helm-git-files:source 'modified))

;;;###autoload
(defvar helm-git-files:untracked-source nil)
(setq helm-git-files:untracked-source
      (helm-git-files:source 'untracked))

;;;###autoload
(defvar helm-git-files:all-source nil)
(setq helm-git-files:all-source
      (helm-git-files:source 'all))

;;;###autoload
(defun helm-git-files:submodule-sources (kinds &optional root)
  (let* ((root (or root (helm-git-files:root)))
         (modules (helm-git-files:submodules root))
         (kinds (if (listp kinds) kinds (list kinds)))
         (once helm-git-files:update-submodules-once))
    (loop for module in modules
          append (loop for what in kinds
                       for path = (file-name-as-directory
                                   (expand-file-name module root))
                       if (file-exists-p path)
                       collect (helm-git-files:source
                                what path module once)))))

;;;###autoload
(defun helm-git-files ()
  "`helm' for opening files managed by Git."
  (interactive)
  (helm-other-buffer `(helm-git-files:modified-source
                           helm-git-files:untracked-source
                           helm-git-files:all-source
                           ,@(helm-git-files:submodule-sources
                              '(modified untracked all)))
                         "*helm for git files*"))

(provide 'helm-git-files)
;;; helm-git-files.el ends here
