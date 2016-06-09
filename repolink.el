;;; repolink.el --- Yank remote repository links from buffer.

;; Author: John Del Rosario <john2x@gmail.com>
;; URL: https://github.com/john2x/repolink
;; Version: 0.0.1-beta

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package contains interactive functions to yank links to a file in
;; a remote repository (e.g. Github).
;; At the moment, only Git is supported.

;;; Code:
(require 'vc-git)

;;;###autoload
(defun repolink ()
  "Get the link of the current file on the remote repository and add it to the kill ring."
  (interactive)
  (let* ((linum (when mark-active (repolink--region-linum)))
         (file-path (repolink--path-to-file))
         (remote-url (repolink--repo-remote-url))
         (branch (repolink--repo-current-branch))
         (link (repolink--build-link remote-url branch file-path linum)))
    (kill-new link)
    (message "Added %s to the kill ring..." link)
    link))

(defun repolink--path-to-file ()
  "Get relative path to file from repository root."
  (let ((fname (buffer-file-name)))
    (file-relative-name fname (locate-dominating-file fname ".git"))))

(defun repolink--region-linum ()
  "Return pair of numbers representing the start and end of a region."
  (save-excursion
    (beginning-of-line)
    (let ((a (1+ (count-lines 1 (region-beginning))))
          (b (count-lines 1 (region-end))))
      (cons a b))))

(defun repolink--repo-remote-url (&optional remote)
  "Get the url of current repo's REMOTE.
If REMOTE is nil, \"origin\" is used."
  (with-temp-buffer
    (vc-git--call t "remote" "-v")
    (goto-char (point-min))
    (let ((remote (or remote "origin"))
          remote-url)
      (while (and (not remote-url) (not (eobp)))
        (when (looking-at (concat "^" remote "[ \t]+\\(.+\\) .*$"))
          (setq remote-url (match-string 1)))
        (forward-line 1))
      remote-url)))

(defun repolink--repo-current-branch ()
  "Get the current branch name."
  (car (vc-git-branches)))

(defun repolink--build-link (remote-url branch file-path &optional linum)
  "Build link from REMOTE-URL, BRANCH, FILE-PATH and LINUM."
  (cond ((repolink--github-p remote-url)
         (repolink--build-github-link remote-url branch file-path linum))
        ((repolink--bitbucket-p remote-url)
         (repolink--build-bitbucket-link remote-url branch file-path linum))))

;; Github specific

(defun repolink--github-p (remote-url)
  "Predicate for checking if REMOTE-URL is from Github."
  (or (string-match-p "^git@github.com" remote-url)
      (string-match-p "^https://github.com" remote-url)))

(defun repolink--region-github-linum (linum)
  "Return a string for Github line highlighting from selected region based on LINUM."
  (concat "#L" (number-to-string (car linum)) "-L" (number-to-string (cdr linum))))

(defun repolink--build-github-link (remote-url branch file-path &optional linum)
  "Build Github link from REMOTE-URL, BRANCH, FILE-PATH and LINUM."
  (concat (repolink--github-host-from-remote-url remote-url)
          "/blob/" branch "/" file-path (when linum (concat (repolink--region-github-linum linum)))))

(defun repolink--github-host-from-remote-url (remote-url)
  "Extract a useable http url from REMOTE-URL."
  (let ((r1 remote-url))
    (when (string-prefix-p "git@" remote-url)
      (setq r1 (replace-regexp-in-string "^git@" "https://" r1))
      (setq r1 (replace-regexp-in-string "\.com:" ".com/" r1)))
    (replace-regexp-in-string "\\.git$" "" r1)))

;; Bitbucket specific

(defun repolink--bitbucket-p (remote-url)
  "Predicate for checking if REMOTE-URL is from Bitbucket."
  (or (string-match-p "^git@bitbucket.org" remote-url)
      (string-match-p "^https://.*\.bitbucket.org" remote-url)))

(defun repolink--region-bitbucket-linum (filename linum)
  "Return a string for Bitbucket line highlighting from selected region based on FILENAME and LINUM."
  (concat "#" filename "-"(number-to-string (car linum)) ":" (number-to-string (cdr linum))))

(defun repolink--build-bitbucket-link (remote-url branch file-path &optional linum)
  "Build Github link from REMOTE-URL, BRANCH, FILE-PATH and LINUM."
  (let ((file-name (car (last (split-string file-path "/")))))
    (concat (repolink--github-host-from-remote-url remote-url)
            "/src/" branch "/" file-path (when linum (concat (repolink--region-bitbucket-linum
                                                              file-name
                                                              linum))))))

(defun repolink--bitbucket-host-from-remote-url (remote-url)
  "Extract a useable http url from REMOTE-URL."
  (let ((r1 remote-url))
    (cond ((string-prefix-p "git@" remote-url)
           (setq r1 (replace-regexp-in-string "^git@" "https://" r1))
           (setq r1 (replace-regexp-in-string "\.org:" ".org/" r1)))
          ((string-prefix-p "https://" remote-url)
           (setq r1 (replace-regexp-in-string "https://.*@?bitbucket" "https://bitbucket" r1))))
    (replace-regexp-in-string "\\.git$" "" r1)))

(provide 'repolink)

;;; repolink.el ends here
