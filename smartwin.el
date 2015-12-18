;;; smartwin.el --- A minor mode for emacs to show shell like buffers. -*- lexical-binding:t -*-

;; Copyright (C) 2015 GuanghuiXu

;; Author: GuanghuiXu gh_xu@qq.com
;; Maintainer: GuanghuiXu gh_xu@qq.com
;; Created: 2015-4-28
;; Keywords: convenience
;; Version: 0.1
;; Homepage: https://github.com/jerryxgh/smartwin
;; Package-Version: 0.1
;; Package-Requires: ((emacs "24"))
;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Smartwin is a window for shell buffers, temp buffers and etc.

;; This minor mode let all all shell like buffers share a window, called smart
;; window, the smart window is always at the bottom of emacs window. Besides
;; that, when point move to the smart window, it is enlarged automaticly, when
;; leave the window, it is shrinked automaticly.

;; To use smartwin, place this file to load path and add this to .emacs:

;; (require 'smartwin)
;; (smartwin-mode 1)
;; or run M-x smartwin-mode to toggle it.

;; Then try run M-x shell or or eshell, if you want to show more buffers in
;; smart window, please customize variable: smartwin-buffers.

;;; Change Log:

;;; TODO:
;; 1. Use new addvice functions to do advice.

;;; Code:

(require 'ido)
(require 'subr-x)

(defgroup smartwin nil
  "A minor mode for emacs to show shell like buffers"
  :prefix "smartwin-"
  :version "0.1.alpha"
  :group 'convenience
  :group 'windows)

(defvar smartwin-previous-buffer nil
  "If smart window is hidden, this variable store previous buffer shown in it.")

(defvar smartwin-max-window-height (+ (/ (frame-height) 2) 2)
  "Maximum hight of smart window.
But smart window can be higher if run `delete-other-window' when is is already
  to this height.")

(defvar smartwin-min-window-height -1
  "Minimal hight of smart window.")

(defcustom smartwin-buffers
  '(;; Emacs
    "*Miniedit Help*"
    completion-list-mode
    compilation-mode
    grep-mode
    occur-mode
    "*scratch*"
    "*evil-registers*"
    "*ielm*"
    "*Inferior Octave*"
    ("^\\*sbt\\*.*" :regexp t)
    "*ensime-db-backtrace-buffer*"
    ;; shell and eshell buffers
    ("^\\*e?shell\\*\\(<.*>\\)?$" :regexp t)
    "*Pp Macroexpand Output*"
    "*Shell Command Output*"
    ;; VC
    "*vc-diff*"
    "*vc-change-log*"
    ;; Undo-Tree
    " *undo-tree*"
    ;; geiser
    " Chicken REPL *"
    ;; Anything
    ("^\\*anything.*\\*$" :regexp t)
    ;; SLIME
    "*slime-apropos*"
    "*slime-macroexpansion*"
    "*slime-description*"
    "*slime-compilation*"
    "*slime-xref*"
    sldb-mode
    slime-repl-mode
    slime-connection-list-mode)
  "Configuration of buffers to be shown in smart window."
  :type '(repeat
          (cons :tag "Config"
                (choice :tag "Pattern"
                        (string :tag "Buffer Name")
                        (symbol :tag "Major Mode"))
                (plist :tag "Keywords"
                       :value (:regexp nil)
                       :options
                       ((:regexp (boolean :tag "On/Off"))))))
  :get (lambda (symbol)
         (mapcar (lambda (element)
                   (if (consp element)
                       element
                     (list element)))
                 (default-value symbol)))
  :group 'smartwin)

(defun smartwin--match-buffer (buffer-or-name)
  "Judge whether to use smartwin to show BUFFER-OR-NAME."
  (let ((buffer (get-buffer buffer-or-name)))
    (if buffer
        (let ((name (buffer-name buffer))
              (mode (buffer-local-value 'major-mode buffer)))
          (catch 'break
            (dolist (config smartwin-buffers)
              (let ((pattern (if (consp config) (car config) config))
                    (keywords (if (consp config) (cdr config) nil)))
                (if (cond ((eq pattern t) (not (buffer-file-name buffer)))
                          ((and (stringp pattern) (plist-get keywords :regexp))
                           (string-match pattern name))
                          ((stringp pattern)
                           (string= pattern name))
                          ((symbolp pattern)
                           (eq pattern mode))
                          ((functionp pattern)
                           (funcall pattern buffer))
                          (t (error "Invalid pattern: %s" pattern)))
                    (throw 'break config)))))))))

(defun smartwin--smart-window-p (window)
  "To judge whether WINDOW is smart window or not."
  (and window (windowp window) (window-parameter window 'smartwinp)))

(defun smartwin--enlarge-window (window)
  "Try to enlarge smart WINDOW, but not too large."
  (let ((height-before (window-height window))
        (window-start (window-start))
        (window-end (window-end)))
    (if (not (string-prefix-p "*helm" (buffer-name)))
        (fit-window-to-buffer window
                              smartwin-max-window-height
                              smartwin-min-window-height))
    ;; set smart window start
    (if (> (window-height window) height-before)
        (let ((forward-line-num (- height-before (window-height window)))
              left-to-forward)
          (set-window-start
           window
           (let (return done)
             (while (not done)
               (save-excursion
                 (goto-char window-start)
                 (setq left-to-forward (forward-line forward-line-num))
                 (setq return (window-point window))
                 (if (or (eq window-end (window-end))
                         left-to-forward
                         (>= forward-line-num 0))
                     (setq done t)
                   (setq forward-line-num (+ forward-line-num 1)))
                 ))
             return)
           t)))))

(defun smartwin--get-smart-window ()
  "Find smart window among all live windows.
If found, return the window, else return nil."
  (let ((root-tree (car (window-tree (selected-frame)))))
    (if (and (listp root-tree) (car root-tree))
        (let ((bottom-window (car (last root-tree))))
          (if (smartwin--smart-window-p bottom-window)
              bottom-window
            nil))
      nil)))

(defun smartwin--get-befitting-window ()
  "Try to get an existing befitting bottom window as smart window."
  (let ((root-tree (car (window-tree (selected-frame)))))
    (if (and (listp root-tree) (car root-tree))
        (let ((bottom-window (car (last root-tree))))
          (if (and (windowp bottom-window)
                   (<= (window-height bottom-window)
                       (+ smartwin-min-window-height 1)))
              bottom-window
            nil)))))

(defun smartwin--create-befitting-window ()
  "Try to split root window to create smart window."
  (let* ((root-win (frame-root-window))
         (root-height (window-height root-win)))
    (if (frame-parameter nil 'unsplittable)
        nil
      (with-demoted-errors "Warning: %S"
        (if (< smartwin-min-window-height 0)
            (split-window root-win
                          (- root-height
                             smartwin-max-window-height) 'below)
          (split-window root-win
                        (- root-height
                           smartwin-min-window-height) 'below))))))

(defun smartwin--get-or-create-smart-window ()
  "Get or create the smart window.
If succeed, a smart window is returned, else return nil."
  (let ((window (smartwin--get-smart-window)))
    (if window
        window
      (setq window (or (smartwin--get-befitting-window)
                       (smartwin--create-befitting-window)))
      (when window
        (set-window-parameter window 'smartwinp t)
        (set-window-buffer window (or (get-buffer "*scratch*")
                                      (create-scratch-buffer)))
        (set-window-prev-buffers window nil))
      window)))

;; switch-to-buffer-other-window
(defun smartwin--get-largest-window (&optional all-frames dedicated not-selected)
  "Like `get-largest-window', but smart window is excluded.
About ALL-FRAMES, DEDICATED and NOT-SELECTED, please see `get-largest-window'"
  (let ((best-size 0)
        best-window size)
    (dolist (window (window-list-1 nil 'nomini all-frames))
      (when (and (not (smartwin--smart-window-p window))
                 (or dedicated (not (window-dedicated-p window)))
                 (or (not not-selected) (not (eq window (selected-window)))))
        (setq size (* (window-total-size window)
                      (window-total-size window t)))
        (when (> size best-size)
          (setq best-size size)
          (setq best-window window))))
    best-window))

(defun smartwin--get-lru-window (&optional all-frames dedicated not-selected)
  "Like `get-lru-window', but smart window is excluded.
About ALL-FRAMES, DEDICATED and NOT-SELECTED, please see `get-lru-window'"
  (let (best-window best-time second-best-window second-best-time time)
    (dolist (window (window-list-1 nil 'nomini all-frames))
      (when (and (not (smartwin--smart-window-p window))
                 (or dedicated (not (window-dedicated-p window)))
                 (or (not not-selected) (not (eq window (selected-window)))))
        (setq time (window-use-time window))
        (if (or (eq window (selected-window))
                (not (window-full-width-p window)))
            (when (or (not second-best-time) (< time second-best-time))
              (setq second-best-time time)
              (setq second-best-window window))
          (when (or (not best-time) (< time best-time))
            (setq best-time time)
            (setq best-window window)))))
    (or best-window second-best-window)))

(defun smartwin--get-mru-window (&optional all-frames dedicated not-selected)
  "Like `get-mru-window', but smart window is excluded.
About ALL-FRAMES, DEDICATED and NOT-SELECTED, please see `get-mru-window'"
  (let (best-window best-time time)
    (dolist (window (window-list-1 nil 'nomini all-frames))
      (setq time (window-use-time window))
      (when (and (not (smartwin--smart-window-p window))
                 (or dedicated (not (window-dedicated-p window)))
                 (or (not not-selected) (not (eq window (selected-window))))
                 (or (not best-time) (> time best-time)))
        (setq best-time time)
        (setq best-window window)))
    best-window))

;;; work with others
;; ediff
(add-hook 'ediff-before-setup-hook 'smartwin-hide)

;;; advices

(defadvice gdb (before smartwin-before-gdb)
  "When run `gdb', hide smart window."
  (smartwin-hide))

(defadvice mwheel-scroll (around smartwin-around-mwheel-scroll)
  "When scroll window by mouse, let smartwin known."
  (let ((in-smartwin-scroll t))
    ad-do-it))

(defadvice select-window (after smartwin-after-select-window)
  "Enlarge or shringe smart window when select window."
  (if (and (not (boundp 'in-smartwin-select-window))
           (not (boundp 'in-smartwin-scroll)))
      (let ((in-smartwin-select-window t)
            (window (ad-get-arg 0)))
        (if window
            (let ((smart-win (smartwin--get-smart-window)))
              (if smart-win
                  (if (eq window smart-win)
                      (smartwin--enlarge-window smart-win)
                    (if (< smartwin-min-window-height (window-height smart-win))
                        (minimize-window smart-win)))))))))

(defadvice balance-windows (around smartwin-around-balance-windows)
  "When do `balance-windows', ignore smart window."
  (let* ((window (smartwin--get-smart-window))
         (in-smartwin (eq (selected-window) window)))
    (if window
        (if (not in-smartwin)
            (progn
              (smartwin-hide)
              ad-do-it
              (smartwin-show)
              ))
      ad-do-it)))

(defadvice evil-window-move-far-left
    (around smartwin-around-evil-window-move-far-left)
  "When `evil-window-move-far-left', ignore smart window."
  (if (not (smartwin--smart-window-p (selected-window)))
      (if (smartwin--get-smart-window)
          (progn
            (smartwin-hide)
            ad-do-it
            (smartwin-show))
        ad-do-it)
    (message "Attempt to move smart window")))

(defadvice evil-window-move-far-right
    (around smartwin-around-evil-window-move-far-right)
  "When `evil-window-move-far-right', ignore smart window."
  (if (not (smartwin--smart-window-p (selected-window)))
      (if (smartwin--get-smart-window)
          (progn
            (smartwin-hide)
            ad-do-it
            (smartwin-show))
        ad-do-it)
    (message "Attempt to move smart window")))

(defadvice evil-window-move-very-top
    (around smartwin-around-evil-window-move-very-top)
  "When `evil-window-move-very-top', ignore smart window."
  (if (not (smartwin--smart-window-p (selected-window)))
      (if (smartwin--get-smart-window)
          (progn
            (smartwin-hide)
            ad-do-it
            (smartwin-show))
        ad-do-it)
    (message "Attempt to move smart window")))

(defadvice evil-window-move-very-bottom
    (around smartwin-around-evil-window-move-very-bottom)
  "When `evil-window-move-very-bottom', ignore smart window."
  (if (not (smartwin--smart-window-p (selected-window)))
      (if (smartwin--get-smart-window)
          (progn
            (smartwin-hide)
            ad-do-it
            (smartwin-show))
        ad-do-it)
    (message "Attempt to move smart window")))

(defadvice delete-other-windows (around smartwin-around-delete-other-windows)
  "If in smart window, just enlarge it instead of `delete-other-windows'."
  (let ((window (ad-get-arg 0)))
    (if (not window)
        (setq window (selected-window)))
    (if (smartwin--smart-window-p window)
        (smartwin--enlarge-window window)
      (if (or (not (smartwin--get-smart-window)) (eq 2 (length (window-list))))
          ad-do-it
        (smartwin-hide)
        ad-do-it
        (smartwin-show)))))

(defadvice delete-window (around smartwin-around-delete-window)
  "If the window is last oridnary window(not smart window), do not kill it."
  (let ((window (ad-get-arg 0)))
    (if (not window)
        (setq window (selected-window)))
    (if (smartwin--smart-window-p window)
        (progn
          (setq smartwin-previous-buffer (window-buffer window))
          ad-do-it)
      (if (and (smartwin--get-smart-window) (eq 2 (length (window-list))))
          (progn
            (message "Attempt to delete last normal(non smart window) window")
            (setq ad-return-value nil))
        ad-do-it))))

(defadvice get-largest-window (around smartwin-around-get-largest-window)
  "Like `get-largest-window', but exclude smart window."
  (let ((all-frames (ad-get-arg 0))
        (dedicated (ad-get-arg 1))
        (not-selected (ad-get-arg 2)))
    (setq ad-return-value
          (smartwin--get-largest-window all-frames dedicated not-selected))))

(defadvice get-lru-window (around smartwin-around-get-lru-window)
  "Like `get-lru-window', but exclude smart window."
  (let ((all-frames (ad-get-arg 0))
        (dedicated (ad-get-arg 1))
        (not-selected (ad-get-arg 2)))
    (setq ad-return-value
          (smartwin--get-lru-window all-frames dedicated not-selected))))

(defadvice get-mru-window (around smartwin-around-get-mru-window)
  "Like `get-mru-window', but exclude smart window."
  (let ((all-frames (ad-get-arg 0))
        (dedicated (ad-get-arg 1))
        (not-selected (ad-get-arg 2)))
    (setq ad-return-value
          (smartwin--get-mru-window all-frames dedicated not-selected))))

(defadvice split-window (around smartwin-around-split-window)
  "Make smart window unsplittable."
  (let ((window (ad-get-arg 0)))
    (if (smartwin--smart-window-p window)
        (progn (message "Smart window is unsplittable")
               (setq ad-return-value window))
      ad-do-it)))

(defadvice window-splittable-p (around smartwin-around-window-splittable-p)
  "Return nil when parameter of `window-splittable-p' is smart window.
This advice will affect `split-window-sensibly' to make it not
split smart window."
  (let ((window (ad-get-arg 0)))
    (if (smartwin--smart-window-p window)
        (setq ad-return-value nil)
      ad-do-it)))

(defadvice switch-to-buffer (around smartwin-around-switch-to-buffer)
  "When a bffer should be shown in smart window, show it in smart window."
  (let ((buffer-or-name (ad-get-arg 0)))
    (if (smartwin--match-buffer buffer-or-name)
        (let ((scratch-buffer (get-buffer "*scratch*"))
              (cur-buffer (current-buffer)))
          ;; not show smart window when start up
          (if (not (and (eq cur-buffer scratch-buffer)
                        (eq cur-buffer (get-buffer buffer-or-name))
                        (not (buffer-modified-p (get-buffer "*scratch*")))))
              (let ((smart-win (smartwin--get-or-create-smart-window)))
                (with-selected-window smart-win ad-do-it)
                (select-window smart-win))))
      (let ((window (selected-window)))
        (if (smartwin--smart-window-p window)
            (switch-to-buffer-other-window buffer-or-name)
          ad-do-it)))))

(defadvice switch-to-buffer-other-window
    (around smartwin-around-switch-to-buffer-other-window)
  "When a bffer should be show in smart window, chage window to smart window."
  (let ((buffer-or-name (ad-get-arg 0)))
    (if (smartwin--match-buffer buffer-or-name)
        (let ((smart-win (smartwin--get-or-create-smart-window)))
          (with-selected-window smart-win
            ad-do-it)
          (select-window smart-win))
      ad-do-it)))

(defadvice display-buffer-pop-up-window
    (around smartwin-around-display-buffer-pop-up-window)
  "Do not split smart window when run this function."
  (let ((smart-win (smartwin--get-smart-window)))
    (if (not smart-win)
        ad-do-it
      (if (not (window-dedicated-p smart-win))
          (set-window-dedicated-p smart-win t))
      ad-do-it
      (if (window-dedicated-p smart-win)
          (set-window-dedicated-p smart-win nil)))))

(defadvice kill-buffer (around smartwin-around-kill-buffer)
  "Try to insure smart buffer should not shown in normal window and vice versa."
  (let ((smart-win (smartwin--get-smart-window))
        (scratch-buffer (get-buffer "*scratch*"))
        (window (selected-window))
        (buffer-to-kill (or (and (ad-get-arg 0) (get-buffer (ad-get-arg 0)))
                            (current-buffer))))
    (if (eq buffer-to-kill scratch-buffer) ;; not kill scratch buffer, clear it
        (clear-scratch-buffer buffer-to-kill)
      ad-do-it ;; kill buffer
      (if (eq smart-win window)
          ;; auto hide smart window if "*scratch*" is the last smart buffer and
          ;; not been modified
          (progn
            (let ((buffer-list (smartwin--make-smart-buffer-list)))
              (if (and (eq scratch-buffer (window-buffer smart-win))
                       (not buffer-list)
                       (not (buffer-modified-p scratch-buffer)))
                  (smartwin-hide))))
        ;; to insure smart buffer not shown in normal window
        (let ((buffer (window-buffer window)))
          (if (smartwin--match-buffer buffer)
              (let ((normal-buffer-list (smartwin--make-normal-buffer-list)))
                (if normal-buffer-list
                    (set-window-buffer window (car normal-buffer-list))
                  (set-window-buffer window (get-buffer "*scratch*"))))))))))

;;; Minor Mode

(defun smartwin--display-buffer-condition (buffer-or-name action)
  "Part of pair in `display-buffer-alist'.
BUFFER-OR-NAME is a buffer name, ACTION is an argument of `display-buffer'."
  (and (smartwin--match-buffer buffer-or-name) t))

(defun smartwin--display-buffer-action (buffer-or-name alist)
  "Part of pair in `display-buffer-alist'.
BUFFER-OR-NAME is a buffer to display, ALIST is them same form as ALIST."
  (let ((buffer (window-normalize-buffer-to-switch-to buffer-or-name))
        (smart-win (smartwin--get-or-create-smart-window)))
    (with-selected-window smart-win
      (set-window-buffer smart-win buffer))
    smart-win))

;;;###autoload
(define-minor-mode smartwin-mode
  "Toggle smartwin minor mode.
Smartwin is a window for showing shell like buffers, temp buffers and etc."
  :lighter " sw"
  :init-value nil
  :global t
  :group 'smartwin
  ;; The minor mode bindings.
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c s") 'smartwin-switch-buffer)
            map)
  (let ((pair '(smartwin--display-buffer-condition
                smartwin--display-buffer-action)))
    (if smartwin-mode
        (progn
          (push pair display-buffer-alist)
          (ad-activate 'switch-to-buffer)
          (ad-activate 'switch-to-buffer-other-window)
          (ad-activate 'evil-window-move-very-top)
          (ad-activate 'evil-window-move-far-left)
          (ad-activate 'evil-window-move-far-right)
          (ad-activate 'evil-window-move-very-bottom)
          (ad-activate 'split-window)

          (ad-activate 'display-buffer-pop-up-window)
          (ad-activate 'window-splittable-p)
          (ad-activate 'get-largest-window)
          (ad-activate 'get-lru-window)
          (ad-activate 'get-mru-window)
          (ad-activate 'delete-window)
          (ad-activate 'delete-other-windows)
          ;; (ad-activate 'balance-windows)
          (ad-activate 'mwheel-scroll)
          (ad-activate 'select-window)
          (ad-activate 'kill-buffer)
          (ad-activate 'gdb)
          ;; (add-hook 'kill-emacs-hook 'smartwin-hide)
          (create-scratch-buffer)

          ;; detect appropriate minimal height of smart window
          (when (< smartwin-min-window-height 0)
            (smartwin-show)
            (let ((window (smartwin--get-smart-window)))
              (minimize-window window)
              (setq smartwin-min-window-height (window-height window)))
            (smartwin-hide)
            (setq smartwin-previous-buffer nil))

          (if (buffer-live-p smartwin-previous-buffer)
              (smartwin-show)))

      ;; (remove-hook 'kill-emacs-hook 'smartwin-hide)
      (setq display-buffer-alist (delete pair display-buffer-alist))
      (ad-deactivate 'switch-to-buffer)
      (ad-deactivate 'switch-to-buffer-other-window)
      (ad-deactivate 'evil-window-move-very-top)
      (ad-deactivate 'evil-window-move-far-left)
      (ad-deactivate 'evil-window-move-far-right)
      (ad-deactivate 'evil-window-move-very-bottom)
      (ad-deactivate 'split-window)
      (ad-deactivate 'display-buffer-pop-up-window)
      (ad-deactivate 'window-splittable-p)
      (ad-deactivate 'get-largest-window)
      (ad-deactivate 'get-lru-window)
      (ad-deactivate 'get-mru-window)
      (ad-deactivate 'delete-window)
      (ad-deactivate 'delete-other-windows)
      (ad-deactivate 'balance-windows)
      (ad-deactivate 'mwheel-scroll)
      (ad-deactivate 'select-window)
      (ad-deactivate 'kill-buffer)
      (ad-deactivate 'gdb)
      (define-key lisp-interaction-mode-map (kbd "C-x k") nil)

      (smartwin-hide))))

(defun smartwin-show ()
  "Show smart window."
  (interactive)
  (if (and smartwin-mode
           (not (smartwin--get-smart-window)))
      (let ((window (smartwin--get-or-create-smart-window)))
        (if (and smartwin-previous-buffer
                 (buffer-live-p smartwin-previous-buffer))
            (if (not (eq smartwin-previous-buffer (window-buffer window)))
                (set-window-buffer window smartwin-previous-buffer))))))

(defun smartwin-hide ()
  "Hide smart window, store its buffer as previous buffer."
  (interactive)
  (let ((window (smartwin--get-smart-window)))
    (when window
      (setq smartwin-previous-buffer (window-buffer window))
      (delete-window window))))

(defun smartwin--make-smart-buffer-list ()
  "Return smart buffer list that not shown in smartwin."
  (let ((visible-buffers (ido-get-buffers-in-frames 'current)))
    (delq nil
          (mapcar
           (lambda (x)
             (let ((name (buffer-name x)))
               (if (and (smartwin--match-buffer x)
                        (not (member name visible-buffers)))
                   name)))
           (buffer-list)))))

(defun smartwin--make-normal-buffer-list ()
  "Return list of normal buffers that not shown."
  (let ((visible-buffers (ido-get-buffers-in-frames 'current)))
    (delq nil
          (mapcar
           (lambda (x)
             (let ((name (buffer-name x)))
               (if (and (not (smartwin--match-buffer x))
                        (not (member name visible-buffers))
                        (not (string-match "^ ?\\*" name)))
                   name)))
           (buffer-list)))))

(defun smartwin-switch-buffer ()
  "Pop buffer that can be showed in smartwin.
This function get input by ido."
  (interactive)
  (let* ((smartwin-buffers (smartwin--make-smart-buffer-list))
         (chosen (and smartwin-buffers
                      (ido-completing-read "Smartwin:" smartwin-buffers)))
         (buffer (and chosen
                      (not (string-empty-p chosen))
                      (get-buffer chosen))))
    (if (not buffer)
        (if (not smartwin-buffers)
            (progn
              (message
               "Then only one smartwin buffer has been shown, no other buffers")
              (select-window (smartwin--get-or-create-smart-window)))
          (message (format "Buffer %s not exist" chosen)))
      (switch-to-buffer buffer))))

(defun create-scratch-buffer ()
  "Create a scratch buffer with the scratch message."
  (interactive)
  (unless (get-buffer "*scratch*")
    (with-current-buffer (get-buffer-create "*scratch*")
      (progn
        (insert initial-scratch-message)
        (goto-char (point-max))
        (lisp-interaction-mode)
        (current-buffer)))))

(defun clear-scratch-buffer (&optional buffer-or-name)
  "Clear the scratch buffer and keep the scratch message.
If BUFFER-OR-NAME is not nil, treat is as scratch buffer."
  (interactive)
  (let ((buffer (if (not buffer-or-name)
                    (get-buffer "*scratch*")
                  (get-buffer buffer-or-name))))
    (when (eq buffer (current-buffer))
      (delete-region (point-min) (point-max))
      (insert initial-scratch-message)
      (goto-char (point-max)))))


(provide 'smartwin)

;;; smartwin.el ends here
