
(setq default-directory "~/")
(defconst lisp-path "~/.emacs.d/site-lisp/")
(add-to-list 'load-path lisp-path)

(prefer-coding-system 'utf-8-unix)
;; (set-default-font "Consolas-12")
(require 'linum)
(global-linum-mode t)
;; (setq linum-format "%4d ")
;; (if window-system
;;     (tool-bar-mode nil))
(when (functionp 'tool-bar-mode)
  (tool-bar-mode 0))
;; (menu-bar-mode 0)
(column-number-mode t)
(size-indication-mode t)
(cua-mode 0)
(transient-mark-mode t)
(which-function-mode t)
;; (ido-mode t)
;; (desktop-save-mode t)
(global-auto-revert-mode)
(pending-delete-mode t)
(setq appt-issue-message t)
(setq auto-save-default nil)
(setq vc-follow-symlinks t)
(setq x-select-enable-clipboard t)
;; (setq show-paren-style 'parenthesis)
;; (setq show-paren-delay 0)
(show-paren-mode t)
(set-face-bold-p 'show-paren-match t)
(set-face-foreground 'show-paren-match "red")
(set-face-background 'show-paren-match nil)
(setq line-move-visual nil)
;; (setq frame-title-format "%b - %F")
(setq frame-title-format '("%S" (buffer-file-name "%f" (dired-directory dired-directory "%b"))))
(setq default-frame-alist '((height . 38) (width . 110)))
(setq inhibit-startup-message t)
(setq default-major-mode 'text-mode)
(setq-default make-backup-files nil)
(setq-default show-trailing-whitespace t)   ; whitespace-cleanup
;; do not show trailing whitespace in these mode
(add-hook 'eshell-mode-hook             ; TODO: calendar-mode-hook, help-mode-hook
          '(lambda() (setq show-trailing-whitespace nil)))
(setq require-final-newline t)          ; TODO: didn't work in php-mode
(setq-default indicate-buffer-boundaries 'left)
(setq-default cursor-type 'bar)
;; (customize-set-variable 'scroll-bar-mode 'left)
;; at begin of line, `kill-line` kills the whole line
;; (setq-default kill-whole-line t)
(setq-default tab-width 4)
(setq tab-stop-list (mapcar (lambda (x) (* x tab-width))
                            (number-sequence 1 40)))
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 4)
(fset 'yes-or-no-p 'y-or-n-p)
;; (mouse-avoidance-mode 'animate)
(setq-default line-spacing 3)
;; (setq display-time-24hr-format t)
;; (setq display-time-day-and-date t)
(display-time-mode 1)


(put 'narrow-to-region 'disabled nil)   ; C-x n n / C-x n w
(put 'upcase-region 'disabled nil)      ; C-x C-u
(put 'downcase-region 'disabled nil)    ; C-x C-l


(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key "\M-g" 'goto-line)      ; override M-g n / M-g p
;; (global-set-key (kbd "C-x k") 'kill-this-buffer)


;; prevent emacs asking "Modified buffers exists; exit anyway?"
(defadvice save-buffers-kill-emacs (around no-y-or-n activate)
  (flet ((yes-or-no-p (&rest args) t)
         (y-or-n-p (&rest args) t))
    ad-do-it))

;; copy current line
(defadvice kill-ring-save (before slickcopy activate compile)
  " without selection"
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2))))
  )

;; toogles the comment state of lines (default to current line)
(defun comment-dwim-line (&optional arg)
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (or (not (looking-at-p "[ \t]*$")) (not (= (preceding-char) ?\x20))))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position arg))
    (comment-dwim arg)))
;; (global-set-key (kbd "M-;") 'comment-dwim-line) ; (kbd "M-;") = "\M-;"
(global-set-key [remap comment-dwim] 'comment-dwim-line)

;; begin a new line below the cursor
(defun begin-new-line nil
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key [C-return] 'begin-new-line)

;; fix backword-kill-word
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Regexp-Backslash.html#Regexp-Backslash
(defun my-backward-kill-word (&optional arg)
  (interactive "*p")
  (if (looking-back "[ \t]" 1)
      (while (looking-back "[ \t]" 1)
        (backward-delete-char 1))
    (backward-kill-word arg)))
(global-set-key [remap backward-kill-word] 'my-backward-kill-word)
(fset 'aquamacs-backward-kill-word 'my-backward-kill-word)

(if (eq system-type "gnu/linux")
    (defun my-fullscreen ()
      (interactive)
      (x-send-client-message
       nil 0 nil "_NET_WM_STATE" 32
       '(2 "_NET_WM_STATE_FULLSCREEN" 0)))
  (global-set-key [f11] 'my-fullscreen))


;; eshell
;; open file in emacs
(defalias 'eshell/em 'find-file)


(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; session
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;; redo+
(require 'redo+)
(global-set-key (kbd "C-?") 'redo)

(require 'php-mode)

(autoload 'markdown-mode "markdown-mode.el"
          "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.\\(md\\|mkd\\|markdown\\)$" . markdown-mode))

;; recent
;; (require 'recentf)
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; color-theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-euphoria)

(require 'server)
(unless (server-running-p)
  (server-start))


;; http://www.emacswiki.org/emacs/CustomizeAquamacs
(when (featurep 'aquamacs)
  ;; transparency
  (setq transparency-level 80)
  (set-frame-parameter nil 'alpha transparency-level)
  (add-hook 'after-make-frame-functions
            (lambda (selected-frame)
              (set-frame-parameter selected-frame 'alpha transparency-level)))
  ;; switch to fullscreen mode
  ;; (aquamacs-toggle-full-frame)
  (setq inhibit-startup-echo-area-message t))

(if (file-exists-p "~/.local.emacs")
    (load "~/.local.emacs"))

;; vim: ft=lisp
