
(setq default-directory "~/")
(defconst lisp-path "~/.emacs.d/site-lisp/")
(add-to-list 'load-path lisp-path)

;; (set-language-environment 'UTF-8)
;; (set-default-font "Consolas-12")
(require 'linum)
(global-linum-mode t)
(if window-system
    (tool-bar-mode nil))
;; (menu-bar-mode nil)
(column-number-mode t)
(size-indication-mode t)
(transient-mark-mode t)
(which-function-mode t)
;; (desktop-save-mode t)
(pending-delete-mode t)
(setq appt-issue-message t)
(setq auto-save-default nil)
(setq vc-follow-symlinks t)
(setq x-select-enable-clipboard t)
;; (setq show-paren-style 'parenthesis)
;; (setq show-paren-delay 0)
(show-paren-mode t)
(setq line-move-visual nil)
;; (setq frame-title-format "%b - %F")
(setq frame-title-format '("%S" (buffer-file-name "%f" (dired-directory dired-directory "%b"))))
(setq default-frame-alist '((height . 38) (width . 110)))
(setq inhibit-startup-message t)
(setq default-major-mode 'text-mode)
(setq-default make-backup-files nil)
;; at begin of line, `kill-line` kills the whole line
;; (setq-default kill-whole-line t)
(setq-default tab-width 4)
(setq tab-stop-list nil)
(setq tab-stop-list (mapcar (lambda (x) (* x tab-width))
                            (number-sequence 1 40)))
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 4)
(fset 'yes-or-no-p 'y-or-n-p)
;; (mouse-avoidance-mode 'animate)
(setq-default line-spacing 3)
(setq display-time-24hr-format t)
;; (setq display-time-day-and-date t)
(display-time-mode 1)
;; (setq linum-format "%4d ")


;; (global-set-key (kbd "C-x k") 'kill-this-buffer)

;; copy current line
(defadvice kill-ring-save (before slickcopy activate compile)
  " without selection"
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2))))
  )

;; toogles the comment state of current line
(defun comment-dwim-line (&optional arg)
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key (kbd "M-;") 'comment-dwim-line) ;; (kbd "M-;") = "\M-;"

(if (eq system-type "gnu/linux")
    (defun my-fullscreen ()
      (interactive)
      (x-send-client-message
       nil 0 nil "_NET_WM_STATE" 32
       '(2 "_NET_WM_STATE_FULLSCREEN" 0)))
  (global-set-key [f11] 'my-fullscreen))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; tabbar
(when (require 'tabbar nil t)
  (setq tabbar-buffer-groups-function
        (lambda (b) (list "All Buffers")))
  (setq tabbar-buffer-list-function
        (lambda()
          (remove-if
            (lambda(buffer)
              (find (aref (buffer-name buffer) 0) " *"))
            (buffer-list))))
  (tabbar-mode))
;; (global-set-key (kbd "") 'tabbar-backward-group)
;; (global-set-key (kbd "") 'tabbar-forward-group)
(global-set-key (kbd "C-`") 'tabbar-backward)
(global-set-key (kbd "<C-tab>") 'tabbar-forward)
(set-face-attribute 'tabbar-unselected-face nil
                    :inherit 'tabbar-default
                    :box '(:color "#00B2BF"))
(set-face-attribute 'tabbar-selected-face nil
                    :inherit 'tabbar-default
                    :background "#D7A8FF"
                    :box '(:color "#00B2BF"))


;; session
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;; redo+
(require 'redo+)
(global-set-key (kbd "C-?") 'redo)

(require 'php-mode)
(define-key c-mode-base-map [(return)] 'newline-and-indent)

(autoload 'markdown-mode "markdown-mode.el"
          "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.mkd" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))

;; recent
;; (require 'recentf)
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "C-x r") 'recentf-open-files-complete)

;; color-theme
(require 'color-theme)
(color-theme-euphoria)

;; vim: ft=lisp