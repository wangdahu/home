
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
(dolist (hook (list 'eshell-mode-hook 'calendar-mode-hook 'help-mode-hook))
  (add-hook hook '(lambda () (setq show-trailing-whitespace nil))))
(setq require-final-newline t)
(add-hook 'php-mode-hook
          '(lambda () (setq require-final-newline t)))
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


(defadvice save-buffers-kill-emacs (around no-y-or-n activate)
  "prevent emacs asking 'Modified buffers exists; exit anyway?'"
  (flet ((yes-or-no-p (&rest args) t)
         (y-or-n-p (&rest args) t))
    ad-do-it))

(defadvice kill-ring-save (before slickcopy activate compile)
  "copy current line if no region active"
  (interactive
   (if mark-active
       (progn (message "range yanked") (list (region-beginning) (region-end)))
     (message "%s line(s) yanked" 1)
     (list (line-beginning-position) (line-beginning-position 2)))))

(defun my-comment-dwim (&optional arg)
  "toogles the comment state of lines (default to current line)"
  (interactive "*P")
  (comment-normalize-vars)
  (if (or (region-active-p)
          (and (looking-at-p "[ \t]*$")
               (or (= (preceding-char) 10) (= (preceding-char) ?\x20))))
      (comment-dwim arg)
    (comment-or-uncomment-region (line-beginning-position) (line-end-position arg))))
;; (global-set-key (kbd "M-;") 'my-comment-dwim) ; (kbd "M-;") = "\M-;"
(global-set-key [remap comment-dwim] 'my-comment-dwim)

(defun begin-new-line nil
  "begin a new line below the cursor"
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key [C-return] 'begin-new-line)

(defun delete-chars (chars &optional backward)
  (delete-region (point)
                 (progn (if backward (skip-chars-backward chars) (skip-chars-forward chars)) (point))))

(defun my-kill-word (&optional arg)
  "fixed kill-word killing whitespace"
  (interactive "*p")
  (if (looking-at-p "[ \t]")
      (delete-chars " \t")
      (kill-word arg)))
;; TODO: kill filename in minibuffer
(global-set-key [remap kill-word] 'my-kill-word)
(fset 'aquamacs-kill-word 'my-kill-word)

;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Regexp-Backslash.html#Regexp-Backslash
(defun my-backward-kill-word (&optional arg)
  "fixed backward-kill-word killing whitesapce"
  (interactive "*p")
  (if (looking-back "[ \t]" 1)
      (delete-chars " \t" t)
    (backward-kill-word arg)))
(global-set-key [remap backward-kill-word] 'my-backward-kill-word)
(fset 'aquamacs-backward-kill-word 'my-backward-kill-word)

(defun kill-other-buffers()
  (interactive)
  (dolist (buf (cdr (buffer-list)))
    (kill-buffer buf)))
(defalias 'only 'kill-other-buffers)


;; eshell
;; open file in emacs
(setq eshell-directory-name "~/.emacs.d/.eshell")
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

(if (file-exists-p "~/.emacs.local")
    (load "~/.emacs.local"))

;; vim: ft=lisp
