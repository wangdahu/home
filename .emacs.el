;; emacs 配置文件

;; 配置初始文件路径/所调用的文件路径
(setq default-directory "/var/www/")
(add-to-list 'load-path' "~/.emacs.d/site-lisp")

;; M-x el 打开 emacs配置文件
(defun el()
  (interactive)
  (find-file user-init-file))

;; 字体控制
(set-default-font "FreeMono-12")

;; 默认模式
(setq default-major-mode 'text-mode)

;; 方便模式切换
(defalias 'h 'html-mode)
(defalias 'p 'php-mode)
(defalias 'j 'js-mode)
(defalias 'c 'css-mode)

;; eshell 下直接打开文件
(defalias 'eshell/em 'find-file)

;; M-x e->eshell
(defalias 'e 'eshell)
;; 关闭当前窗口(一般乍关闭错误的信息用)
(defalias 'x 'delete-window)
(defalias 'z 'kill-this-buffer)

;; 改变eshell标题， 去除××
(setq eshell-buffer-name "eshell")

;; 有 emacsclient 请求时，自动激活 emacs 窗口
(add-hook 'server-visit-hook
          '(lambda ()
             (setq server-window (make-frame))))

;; 退出 emacs 时，自动关闭当前 buffer
(add-hook 'server-done-hook
          '(lambda ()
             (delete-frame server-window)
             (setq server-window nil)))

;; 查找文件
(add-to-list 'load-path "~/.emacs.d/site-lisp/fuzzy-find-in-project")
(require 'fuzzy-find-in-project)
(fuzzy-find-project-root "/var/www")
(global-set-key (kbd "C-c C-f") 'fuzzy-find-in-project)

;; 设置显示时间
(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)

;; 不自动备份文件
(setq make-backup-files nil)

;; 显示行号
(global-linum-mode t)
(setq column-number-mode t)
(setq linum-format " %d ")

;; 关闭启动emacs时的画面,不设置,这里直接影响在这里着文件的路径(C-x C-f 这时设置的default-direactory就不起作用了)
(setq inhibit-startup-message t)
;; (setq gnus-inhibit-startup-message t)

;; 显示括号匹配
(show-paren-mode t)
(setq show-paren-style 'parentheses)
(setq line-move-visual nil)
(setq show-paren-delay 0)
(set-face-foreground 'show-paren-match "red")
(set-face-bold-p 'show-paren-match t)
(set-face-background 'show-paren-match nil)

;; 高亮行
(require 'hl-line)
(global-hl-line-mode t)
(or (facep 'my-hl-line-face) (make-face 'my-hl-line-face))
(setq hl-line-face 'my-hl-line-face)
(face-spec-set 'my-hl-line-face '((t (:background "#000000"))))

;; 当 % 在括号上按下时，那么匹配括号，否则输 入一个 %。
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
 ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
 (t (self-insert-command (or arg 1)))))

;; 括号自动补全
( setq skeleton-pair-alist nil)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "<") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)


;; desktop (保存桌面,回到上次打开的文件)
(desktop-save-mode t)
(setq desktop-load-locked-desktop nil)
(add-hook 'desktop-not-loaded-hook 'desktop-save-mode-off)

;; 允许emacs和其他外部程序的粘贴
(setq x-select-enable-clipboard t)

;; 若命令有组合建,则提示该组合建
(setq suggest-key-bindings t)


;; 文件名的标题栏
(setq frame-title-format "%b - %F")

;; 加载颜色配置
(require 'color-theme)
(color-theme-euphoria)
;; (color-theme-arjen)

;; 开启时不加载工具栏
(tool-bar-mode nil)
;; (menu-bar-mode nil)

;; 加载php模式
(require 'php-mode)
(add-hook 'php-mode-hook
          '(lambda ()
            (setq require-final-newline t
                  comment-start "//" comment-end "")
            (defun ywb-php-lineup-arglist-intro (langelem)
              (save-excursion
                (goto-char (cdr langelem))
                (vector (+ (current-column) c-basic-offset))))
            (defun ywb-php-lineup-arglist-close (langelem)
              (save-excursion
                (goto-char (cdr langelem))
                (vector (current-column))))
            (c-set-offset 'arglist-intro 'ywb-php-lineup-arglist-intro)
            (c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close)))

;; 自动填充
;; (setq-hook 'text-mode-hook 'turn-on-auto-fill)
(setq default-fill-column 120)


;; 一个tab为四个空格,用空格代替tab
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq c-basic-offset 4)
(setq html-basic-offset 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96))

;; 回车缩进
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key (kbd "C-<return>") 'newline)


;; 把yes和no 用y和n 代替
(fset 'yes-or-no-p 'y-or-n-p)


;; 让顶部有多个选项卡
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
;; 切换顶部选项卡的快捷键
(global-set-key [(\C-tab)] 'tabbar-backward)
(global-set-key (kbd "C-`") 'tabbar-forward)


;;;; 设置tabbar外观
;; 设置默认主题: 字体, 背景和前景颜色，大小

(set-face-attribute 'tabbar-unselected-face nil
                    :inherit 'tabbar-default
                    :box '( :color "#00B2BF")
                    )
(set-face-attribute 'tabbar-selected-face nil
                    :inherit 'tabbar-default
                    :background "#D7A8FF"
                    :box '( :color "#00B2BF")
                    )

;; 允许emacs和外部其他程序的粘贴
(setq x-select-enable-clipboard t)

;; 关闭缓冲区不用回车
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "s-w") 'kill-this-buffer)


;; 缓冲区的加强版
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; 光标设置未竖线
(setq-default cursor-type 'bar)
;; (setq-default cursor-type 'box)
;; 光标的颜色设置未红色
(set-mouse-color "red")

;; 相同的的文件名显示出不同的路径
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; 标题栏显示路径
(setq frame-title-format
      '("%S" (buffer-file-name "%f"
                               (dired-directory dired-directory "%b"))))

;; 可以粘贴时删除标记的字符串
(pending-delete-mode t)

;; 滚动条放在右边
(customize-set-variable 'scroll-bar-mode 'right)

;; 自动加载修改国的文件
(global-auto-revert-mode)


;; 过滤不想看到的文件类型
(setq dired-omit-extensions '(".svn-base"))

;; 复制当前行
(defadvice kill-ring-save (before slickcopy activate compile)
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; 光标在行中时添加注释和删除注释
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

;; 添加一行,不管光标位置
(defun create-new-line-no-matter nil
  (interactive)
  (end-of-line)
  (newline-and-indent))
;; 不管光标位置,添加在上一行新增一行
(defun create-previous-line nil
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line))
(global-set-key [C-return] 'create-new-line-no-matter)
(global-set-key [M-return] 'create-previous-line)

;; 复制当前文件名
(defun copy-file-name (&optional full)
  (interactive "P")
  (let ((file (buffer-name)))
    (if full
        (setq file (expand-file-name file)))
    (kill-new file)
    (message
     "File `%s' copied." file)))
(global-set-key (kbd "C-c M-c" ) 'copy-file-name)

;; 查找文件的功能扩展
(ido-mode t)

;; 删除环的列表操作
(require 'browse-kill-ring)
(global-set-key (kbd "C-c k") 'browse-kill-ring)
(browse-kill-ring-default-keybindings)


;; 最近打开文件记录
(require 'recentf-settings)
(setq recentf-auto-clearup 'never)
(setq recentf-max-menu-items 200)

;; eshell中输入cls清空eshell
(defun eshell/cls()
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (point-min) (point-max))))

;; eshell 下用em直接打开文件
(defalias 'eshell/em 'find-file)

;; eshell 复制和打开当前光标的文件
(defun get-file(operation)
  (let* ((file (thing-at-point 'filename))
         (path (if (and file (> (length file) 0)) (expand-file-name file) nil)))
    (if (and path (file-exists-p path))
        (funcall operation path)
      (message "NO Filename Found At Point"))))

(add-hook 'eshell-mode-hook
          (lambda()
            (local-set-key [C-return] '(lambda()
                                         (interactive) (get-file 'find-file)))
            (local-set-key [M-return] '(lambda()
                                         (interactive) (get-file 'kill-new)))))

;; 跳转行
(global-set-key (kbd "M-g") 'goto-line)

(setq mouse-autoselect-window t)

;; 标记当前位置,再回到当前位置
(global-set-key (kbd "C-<") 'ska-point-to-register)
(global-set-key (kbd "C-,") 'ska-jump-to-register)
(defun ska-point-to-register()
  "Store cursorposition _fast_ in a register.
  Use ska-jump-to-register to jump back to the stored
  position."
  (interactive)
  (setq zmacs-region-stays t)
  (point-to-register 8))
(defun ska-jump-to-register()
  "Switches between current cursorposition and position
  that was stored with ska-point-to-register."
  (interactive)
  (setq zmacs-region-stays t)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp)))


;; 复制当前行到下一行
(defun copy-line-down()
  (interactive)
  (let ((c (current-column)))
    (kill-whole-line)
    (yank)
    (yank)
    (previous-line 1)
    (move-to-column c)))
(global-set-key (kbd "C-M-n") 'copy-line-down)


;; 重新绑定C-z到撤销(根据windows习惯)
(global-set-key (kbd "C-z") 'undo)

;; 显示出文件中多余的空格
(setq-default show-trailing-whitespace t)
(global-set-key (kbd "C-c l") 'whitespace-cleanup)

;; 全屏设置
(if (eq system-type "gnu/linux")
    (defun my-fullscreen ()
      (interactive)
      (x-send-client-message
       nil 0 nil "_NET_WM_STATE" 32
       '(2 "_NET_WM_STATE_FULLSCREEN" 0)))
  (global-set-key [f11] 'my-fullscreen))

;; 关闭其他标签
(defun kill-other-buffers(&optional list)
  (interactive)
  (dolist (buff (cdr (buffer-list)))
    (kill-buffer buff)))
(global-set-key (kbd "C-c o") 'kill-other-buffers)


;; 显示空白字符
(setq-default show-trailing-whitespace t)
(global-set-key (kbd "C-c l") 'whitespace-cleanup)

;; 各种模式下 不显示空白的字符
(dolist (hook '(eshell-mode-hook calendar-mode-hook help-mode-hook))
  (add-hook hook '(lambda() (setq show-trailing-whitespace nil))))

;; 单个字母查找
(defun wy-go-to-char(n char)
  (interactive "p\ncGo to char:")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char)
                     char)
    (search-forward (string char) nil nil n))
  (setq unread-command-events (list lasr-input-event)))
(global-set-key (kbd "C-c a") 'wy-go-to-char)

;; 配置cedet
;; Ecb的操作:
;; C-c . g d 目录列表窗口
;; C-c . g s 源码窗口
;; C-c . g m 方法和变量窗口
;; C-c . g h 历史窗口
;; C-c . g l 最后选择过的编辑窗口
;; C-c . g 1 编辑窗口1
;; C-c . g n 编辑窗口n
;; C-c . l c 选择版面
;; C-c . l r 重画版面
;; C-c . l t 拴牢版面(锁定版面)
;; C-c . l w 拴牢可见的ecb窗口
;; C-c . \ 拴牢编绎窗口
;; (load-file "~/.emacs.d/site-lisp/cedet/common/cedet.el")
;; (semantic-load-enable-code-helpers)
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/ecb")
;; (require 'ecb)
;; ;; 展开ecb的快捷键k
;; (global-set-key (kbd "C-c b") 'ecb-minor-mode)
;; (setq ecb-use-speedbar-instead-native-tree-buffer 'source
;;      ecb-windows-width 0.25
;;      ecb-tip-of-the-day nil
;; )

;; common lisp 的配置
(setq inferior-lisp-program "clisp")


(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.text" . markdown-mode) auto-mode-alist))

;; 跳转至往下某行的倒数第一个字母之前,默认为本行的倒数第一个字母
(defun backwards-end(&optional arg)
  (interactive "p")
  (move-end-of-line arg)
  (backward-char 1))
(global-set-key (kbd "C-c e") 'backwards-end)


;; 这个功能就是根据光标的所在位置，智能的选择一块区域，也就
;; 是设置成为当前的point和mark。这样就可以方便的拷贝或者剪切，或者交换他们的位
;; 置。如果当前光标在一个单词上，那么区域就是这个单词的开始和结尾分别。
;; 如果当前光标在一个连字符上，那么就选择包含连字符的一个标识符。
;; 这个两个的是有区别的，而且根据不同的mode的语法定义，连字符和单词的定义也不一样。
;; 例如C mode下，abc_def_xxx, 如果光标停在abc上，那么就会选择abc这个单词。如果
;; 停在下划线上，那么就会选择abc_def_xxx。
;; 如果当前光标在一个双引号,单引号，一个花括号，方括号，圆括号，小于号，或者大于号，
;; 等等，那么就会选择他们对应的另一个括号之间的区域。引号中的escape字符也是可以
;; 自动识别的。嵌套关系也是可以识别的。这一点可以和VIM中的%的功能类比。
(require 'mouse)
(defun wcy-mark-some-thing-at-point()
  (interactive)
  (let* ((from (point))
         (a (mouse-start-end from from 1))
         (start (car a))
         (end (cadr a))
         (goto-point (if (= from start) end start)))
    (if (eq last-command 'wcy-mark-some-thing-at-point)
        (progn
          ;; exchange mark and point
          (goto-char (mark-marker))
          (set-marker (mark-marker) from))
      (push-mark (if (= goto-point start) end start) nil t)
      (when (and (interactive-p) (null transient-mark-mode))
        (goto-char (mark-marker))
        (sit-for 0 500 nil))
      (goto-char goto-point))))
(define-key global-map (kbd "M-C-l") 'wcy-mark-some-thing-at-point)

;; 复制单词
(defun copy-word (&optional arg)
  "Copy words at point"
 (interactive "P")
 (let ((beg (progn (while (looking-back "[a-zA-Z0-9-_]" 1) (backward-word)) (point)))
       (end (progn (while (looking-at "[a-zA-Z0-9-_]") (forward-word)) (point))))
   (copy-region-as-kill beg end)))

;; 跳转到上次的位置
(require 'recent-jump)
(setq recent-jump-threshold 4)
(setq recent-jump-ring-length 10)
(global-set-key (kbd "<f6>") 'recent-jump-jump-backward)
(global-set-key (kbd "<f7>") 'recent-jump-jump-forward)

;; 自动补全
(add-to-list 'load-path "~/.emacs.d/site-lisp/company")
(autoload 'company-mode "company" nil t)

;; 显示目录
(global-set-key [f11] 'speedbar)

;; 光标在行尾上下移动时,光标始终在行尾
(setq track-eol t)

;; php数组的缩进处理
(add-hook 'php-mode-hook
          (lambda()
            (defun php-arglist-intro(langelem)
              (save-excursion
                (goto-char (cdr langelem))
                (vector (+ (current-column) c-basic-offset))))
            (defun php-arglist-close(langelem)
              (save-excursion
                (goto-char (cdr langelem))
                (vector (current-column))))
            (c-set-offset 'arglist-intro 'php-arglist-intro)
            (c-set-offset 'arglist-close 'php-arglist-close)))

;; 使用M-上/下/左/右切换emacs窗口
(windmove-default-keybindings 'meta)

;; 选中一个区域后,用C-x n n 今日narrow(隐藏其他部分)模式, C-x n w 退出 C-x n d当前函数用narrow

;; 统计某个单词出现的次数 M-x count-natches/ occur(统计出现的次数和相应的位置)

;; emacs 替换^M 的法子,M-x replace-string RET C-q C-m RET RET

(defun toggle-line-spacing()
  "Toggle line spacing between 1 and 5 pixels"
  (interactive)
  (if (eq line-spacing 1)
      (setq-default line-spacing 5)
    (setq-default line-spacing 1)))
(global-set-key (kbd "<f7>") 'toggle-line-spacing)

;; emacs 书签 -> C-x r m, C-x r b, C-x r l

;; emacs 正则  replace-regexp里面可以用正则替换

;; 缩写
(setq-default abbrev-mode t)

;; 查看某个版本的所有文件内容改变
;; svn diff -r 版本号

;; 查看某个版本修改的所有文件
;; svn log -r 版本号 -v
