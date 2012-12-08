;; 在测试运行代码的时候保存,想关闭错误的窗口

;; 打开emacs配置文件
(defun el()
  (interactive)
  (find-file user-init-file))

;; M-x e 打开eshell
(defalias 'e 'eshell)
(defalias 'ec 'eshell-command)
(defalias 'h 'html-mode)
(defalias 'p 'php-mode)
(defalias 'j 'js-mode)
(defalias 'c 'css-mode)

;; 关闭当前的这个窗口,一般做关闭调试信息用
(defalias 'x 'delete-window)

;; 字体设置
(set-default-font "Monospace-14")

(setq default-directory "/var/www/")
(add-to-list 'load-path' "~/.emacs.d/site-lisp")

;; 利用eval-after-load 加快启动速度的库,避免不必要elisp包的加载
(require 'eval-after-load)

;; 关闭缓冲区不需要按回车
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "s-w") 'kill-this-buffer)
(global-set-key (kbd "M-!") 'eshell-command)
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "<s-tab>") 'kill-whole-line)

;; 关闭其他标签栏
(defun kill-other-buffers (&optional list)
  (interactive)
  (dolist (buff (cdr (buffer-list)))
    (kill-buffer buff)))
(global-set-key (kbd "C-c o") 'kill-other-buffers)

;; 设置emacs写文件时的编码
(setq default-buffer-file-coding-system 'utf-8)
;; 设置emacs读文件时的编码
(prefer-coding-system 'utf-8-unix)

(setq default-major-mode 'text-mode)
;; 滚动条放在右边
(customize-set-variable 'scroll-bar-mode 'right)

;; 设置左侧行数字的样子
(global-linum-mode t)
(setq linum-format " %d ")


;; 不显示工具栏
(tool-bar-mode nil)
(menu-bar-mode nil)
(column-number-mode t)
(size-indication-mode t)
(setq auto-save-default nil)

;; 在状态栏上显示当前光标在那个函数体内部
;; (which-function-mode t)

;; 光标显示为一根竖线
(setq-default cursor-type 'bar)

;; 重新绑定C-z到撤销(根据window习惯)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "s-v") 'yank)

;; 不生成备份
(setq-default make-backup-files nil)

;; 设置粘贴缓冲条目数量
(setq kill-ring-max 500)

;; 最后一行加结束换行
(setq require-final-newline t)

;; 括号匹配
(show-paren-mode t)
(setq show-paren-style 'parenthesis)
(setq line-move-visual nil)
(setq show-paren-delay 0)

;; 括号的匹配配置
(set-face-foreground 'show-paren-match "red")
(set-face-bold-p 'show-paren-match t)
(set-face-background 'show-paren-match nil)

;; 高亮编辑行
(require 'hl-line)
(global-hl-line-mode t)
(or (facep 'my-hl-line-face) (make-face 'my-hl-line-face))
(setq hl-line-face 'my-hl-line-face)
(face-spec-set 'my-hl-line-face '((t (
                                      :background "#000000"))))

;; 上次关闭时的列表记录
(desktop-save-mode t)

;; 重名文件则显示出路径
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward) ;post-frowcrmrd

;; 标题栏设置
(setq frame-title-format '(buffer-file-name "%f"))

;; redo+
;; (require 'redo+)
;; (global-set-key (kbd "C-?") 'redo)

;; 开启emacs 不出现图形页面
(setq inhibit-startup-message t)
;; (add-hook 'c-mode-hook 'c-toggle-auto-hungle-state t)

;; 代码缩进
(setq-default tab-width 4)
(setq c-indent-level 4)
(setq indent-line-function 'insert-tab)
(setq c-basic-offset 4)
(setq html-basic-offset 4)
(setq sgml-basic-offset 4)
(setq-default indent-tabs-mode nil)

;; 在行中添加注释和删除注释
(defun comment-dwim-line (&optional arg)
  (interactive "*P")
  (comment-normalize-vars)
  (if (or (region-active-p)
               (and (looking-at "[ \t]*$")
                    (or (= (preceding-char) 10) (= (preceding-char) ?\x20))))
        (comment-dwim arg)
      (comment-or-uncomment-region (line-beginning-position) (line-end-position arg))))
(global-set-key [remap comment-dwim] 'comment-dwim-line)


(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96))

;; 括号自动补全
;; (defun my-php-mode-auto-pair()
;;   (interactive)
;;   (make-local-variable 'skeleton-pair-alist)
;;   (setq skeleton-pair-alist '(
;;                               (?{ \n > _ \n ?} >)))
;;   (setq skeleton-pair t)
;;   (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
;;   (backward-char))
;; (add-hook 'c-mode-hook 'my-php-mode-auto-pair)
;; (add-hook 'c++mode-hook 'my-php-mode-auto-pair)

;; 复制行功能
(defadvice kill-ring-save (before slickcopy activate compile)
  "当没有选中的时候,用M-w来复制当前行"
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defun copy-word(&optional arg)
  "复制单词"
  (interactive "*")
  (let ((word (symbol-at-point)))
        (if (not word)
            (message "no symbol at point")
          (kill-new (format "%s" word))
          (message "`%s'" word)
          )))
(global-set-key (kbd "M-n") 'copy-word)


;; 复制行光标所在位置到行尾
(defun qiang-copy-line (arg)
  (interactive "p")
  (kill-ring-save(point)
                 (line-end-position))
  (message "%d line%s copied " arg(if (= 1 arg) "" "S")))
(global-set-key (kbd "M-k") 'qiang-copy-line)

;; 复制当前行到下一行去
(defun copy-line-down()
  (interactive)
  (let ((c (current-column)))
    (kill-whole-line)
    (yank)
    (yank)
    (previous-line)
    (move-to-column c)))
(global-set-key (kbd "C-M-n") 'copy-line-down)

;; 鼠标滚动的控制
(setq scroll-margin 3
      scroll-conservatively 10000)

;;设置允许emacshe和外部其他程序的粘贴
(setq x-select-enable-clipboard t)

;; 允许鼠标键粘贴
(setq mouse-yank-at-point t)

;; 显示空白区域的空格和换行
;; (whitespace-mode t)

;; 自动加载修改的文件
(global-auto-revert-mode t)

;; 用y,n代替yes,no
(fset 'yes-or-no-p 'y-or-n-p)
;; (setq mouse-avoidance-mode 'animate)
(setq-default line-spacing 5)

;; 时间显示
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time-mode 1)

;; 所在行行高亮
(require 'hl-line)
(hl-line-mode 1)

;; 标记高亮
(transient-mark-mode t)
(put 'narrow-to-region 'disabled nil)

;; tabbar 多标签
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
(global-set-key [(\C-tab)] 'tabbar-forward)
(global-set-key (kbd "C-`") 'tabbar-backward)

;; 设置选项卡的颜色
(set-face-attribute 'tabbar-unselected-face nil
                    :inherit 'tabbar-default
                    :box '(:color "#00B2BF")
                    )
(set-face-attribute 'tabbar-selected-face nil
                    :inherit 'tabbar-default
                    :background "#D7A8FF"
                    :box '(:color "#00B2BF")
                    )

; session
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

(require 'php-mode)

;; 最近打开的文件
(require 'recentf-settings)
(setq recentf-auto-clearup 'never)
(setq recentf-max-menu-items 200)

(setq auto-fill-mode t)
(setq fill-column 120)

;; color-theme
(require 'color-theme)
(color-theme-euphoria)

;; 设置开启Delete_Selection模式(输入替换选中,选中的文字可以被替换掉)
(pending-delete-mode t)

;; 设置其他窗口的向下翻页
(global-set-key (kbd "C-c C-v") 'scroll-other-window)
;; 设置其他窗口的向上翻页
(global-set-key (kbd "C-c C-b") 'scroll-other-window-down)

;; emacs 启动最大化
(setq initial-frame-alist '((top . 1) (left . 640) (width . 93) (height . 51)))

;; 在fringe上显示一个小箭头指示当前buffer的边界
(setq-default indicate-buffer-boundaries 'left)

;; 回车键自动换行并缩进
(global-set-key (kbd "RET") 'newline-and-indent)

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
(define-key global-map (kbd "C-M-l") 'wcy-mark-some-thing-at-point)

;; 复制当前文件名
(defun copy-file-name ()
  (interactive "*")
  (let ((file (buffer-file-name)))
    (kill-new file)
    (message "File `%s' copied." file)))
(global-set-key (kbd "C-c M-c") 'copy-file-name)

;; 缓冲区的加强版
(require 'ibuffer)
(global-set-key [remap list-buffers] 'ibuffer)

;; 查找文件功能扩展
(ido-mode t)

;; 单个字母的查找
(defun wy-go-to-char(n char)
  (interactive "p\ncGo to char:")
  (search-forward (string char) nil t n))
(global-set-key (kbd "C-c a") 'wy-go-to-char)

;; 显示出文件中多余的空格
(setq-default show-trailing-whitespace t)
(global-set-key (kbd "C-c l") 'whitespace-cleanup)

;; 各种模式下 不显示空白字符
(dolist (hook (list 'eshell-mode-hook 'calendar-mode-hook 'help-mode-hook))
  (add-hook hook '(lambda() (setq show-trailing-whitespace nil))))

;; 根据后缀名设置打开文件的mode
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . html-mode))

;; 矩形区域操作
;; (require 'rect-mark-settings)

;; 日历显示(M-x calendar,显示出日历,想爱你日历上点击在按p C 显示出农历的日期)

;; dos-unix
(defun dos-unix()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;; 智能编译(比较智能的C/C++编译命令,如果当前目录有makefile则用make -k 编译,否则,如果是处于c-mode
;; ,就用gcc -Wall编译,如果是c++-mode 就用g++ -Wall编译)
(defun smart-compile()
  (interactive)
  ;; 查找是否存在Makefile
  (let ((candidate-mark-file-name '("markfile" "Makefile" "GNUmakefile"))
        (command nil))
    (if (not (null
              (find t candidate-mark-file-name :key
                    '(lambda (f) (file-readable-p f)))))
        (setq command "make -k")
      ;; 没有找到Makefile,查看当前mode是否是已知的可编译的模式
      (if (null (buffer-file-name (current-buffer)))
          (message "缓冲区文件不存在,不能编译")
        (if (eq major-mode 'c-mode)
            (setq command
                  (concat "gcc -g -Wall -o"
                          (file-name-sans-extension
                           (file-name-nondirectory buffer-file-name))
                          ""
                          (file-name-nondirectory buffer-file-name)
                          ;; "-lm"
                          ))
          (if (eq major-mode 'c++-mode)
              (setq command
                    (concat "g++ -g -Wall -o"
                            (file-name-sans-extension
                             (file-name-nondirectory buffer-file-name))
                            ""
                            (file-name-nondorectory buffer-file-name)
                            ;; "-lm"
                            ))
            (if (eq major-mode 'fortran-mode)
                (setq command
                      (concat "ifort"
                              (file0name-nondirectory buffer-file-name)
                              "-o"
                              ))
              (message "该文件不能编译"))))))
    (if (not (null command))
        (let ((command (read-from-minibuffer "Compile command: " command)))
          (compile command)))))

;; 行跳转快捷键修改
(global-set-key (kbd "M-g") 'goto-line)

;; 改eshell标题，去除**
(setq eshell-buffer-name "eshell")
(setq shell-buffer-name "shell")
;; (require 'multi-term)
;; (setq multi-term-buffer-name "multi-term")

;; 开启另一个eshell窗口
(defun osh()
  (interactive)
  (split-window-vertically)
  (eshell))

;; 两个窗口的内容互换
(defun my-retate-windows()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((wl (window-list))
             (w1 (frame-first-window))
             (w2 (if (equal w1 (car wl)) (cadr wl) (car wl)))
             (b1 (window-buffer w1))
             (b2 (window-buffer w2))
             (first (if (equal (current-buffer) b1) t nil)))
        (if (= (window-width) (frame-width))
            (split-window-horizontally)
          (split-window-vertically))
        (other-window 2)
        (delete-window)
        (switch-to-buffer b1)
        (other-window 1)
        (switch-to-buffer b2)
        (when first (other-window 1)))
    (message "There are not exactly 2 windows .")))

;; 代码补全
(setq hippie-expand-try-function-list
      '(try-expend-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffer
        try-expand-dabbrev-from-fill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))
(global-set-key (kbd "M-/") 'hippie-expand)

;; C-c C-j 打开当前文件的所在目录
(global-set-key (kbd "C-c C-j")
                (lambda ()
                  (interactive)
                  (if (buffer-file-name)
                      (dired default-directory))))

;; 编译并运行python文件
(defun my-python-compile()
  (interactive)
  (compile (concat "python " (buffer-file-name))))
(setq compilation-scroll-output t)
;; (global-set-key (kbd "C-c c") 'my-python-compile)

;; eshell中输入cls清屏
(defun eshell/cls()
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (point-min) (point-max))))

;; ;; 打开文件夹
;; (defun eshell/open(&optional dir)
;;   (interactive)
;;   (eshell-command (concat "nautilus " dir)))

(defun gti (&optional cmd)
  (interactive)
  (concat "git " cmd))

;; eshell 下直接打开文件
(defalias 'eshell/em 'find-file)

;; eshell
(defun get-file (operation)
  (let* ((file (thing-at-point 'filename))
         (path (if (and file (> (length file) 0)) (expand-file-name file) nil)))
    (if (and path (file-exists-p path))
        (funcall operation path)
      (message "No filename found at point"))))

(add-hook 'eshell-mode-hook
          (lambda ()
             (local-set-key [C-return] '(lambda ()
                                          (interactive) (get-file 'find-file)))
             (local-set-key [M-return] '(lambda ()
                                          (interactive) (get-file 'kill-new)))
             ))

;; emacs eshell 中设置环境变量
;; M-x setenv Enter path Enter $path;E:IDKin

;; 光标靠近鼠标,鼠标移动位置
;; (mouse-avoidance-mode 'animate)

;; 新建一行,不管光标位置
(defun create-new-line-no-mater(&optional arg)
  (interactive "P")
  (end-of-line arg)
  (newline))
(global-set-key [C-return] 'create-new-line-no-mater)
(global-set-key [M-return] '(lambda ()(interactive)(create-new-line-no-mater 0)))


;; 两个frame时,鼠标移动到那个frame,光标就在某个frame中了
;; (setq mouse-autoselect-window t)


;; 标记当前位置,在回到当前位置
(defun ska-point-to-register()
  (interactive)
  (setq zmacs-region-stays t)
  (point-to-register 8))
(defun ska-jump-to-register()
  (interactive)
  (setq zmacs-region-stays t)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp)))
(global-set-key (kbd "C-<") 'ska-point-to-register)
(global-set-key (kbd "C-,") 'ska-jump-to-register)

;; markdown
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.text" . markdown-mode) auto-mode-alist))

;; 跳转至某行的倒数第1个字母之前(一般用作跳转到分号之前)
(defun backwards-end(&optional arg)
  (interactive "p")
  (move-end-of-line arg)
  (backward-char 1))
(global-set-key (kbd "C-c e") 'backwards-end)


;; php 数组的缩进处理
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

;; 代替mmm-mode
(defun set-mode ()
  "set multi-mode"
  (interactive)
  (save-excursion
    (if (re-search-backward "</script>\\|\\?>\\|</style>\\|<script[^>]*>\\|<\\?\\|<style[^>]*>\\|<\sw[^>]+>\\(['\"]?\\)" nil t)
        (let ((res (match-string 0))
              (new-mode nil))
          (cond ((equal res "<?") (setq new-mode 'php-mode))
                ((string-match "^<script" res) (setq new-mode 'js-mode))
                ((string-match "^<style" res) (setq new-mode 'css-mode))
                (t (setq new-mode 'html-mode)))
          (and (eq new-mode 'html-mode)
               (> (length (match-string 1)) 0)
               (setq new-mode 'js-mode))
          (or (eq new-mode major-mode)
              (funcall new-mode))))))

(defun php-newline ()
  (interactive "*")
  (if (string-match "views\\(/[a-zA-Z0-9_]+\\)+\\.php$\\|\\.[sp]?html$" buffer-file-truename)
      (set-mode))
  (newline-and-indent))
(global-set-key (kbd "RET") 'php-newline)

(add-to-list 'load-path "~/Documents/find2.el")
(require 'find2)
(require 'find2-find-project)
(setq find2-project-folders '("/var/www/ulife" "/var/www/c" "/var/www/company"))
(setq find2-omit-files (append find2-omit-files (list "company/assets" "company/framework")))
(global-set-key "\C-x\C-a" 'find2)

;; Emacs 替换^M的方法 M-x replace-string RET C-q C-m RET RET

;; 光标早行尾上下移动的时候,始终保持在行尾
(setq track-eol t)

;; 显示目录
(global-set-key [f11] 'speedbar)
(setq speedbar-show-unknown-files t)
(setq speedbar-use-images nil)

;; 选中一个区域后,用C-x n n 进入narrow(隐藏其他部分)模式, C-x n w 退出narrow模式
;; C-x n d 当前行所在的函数用 narrow模式编辑

;; 统计某个单词出现的次数 M-x count-natches
;; M-x occur 统计表达式出现的次数和相应的位置

;; 自动补全
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/company")
;; (autoload 'company-mode "company" nil t)

;; 打开文件时回到上次退出时的光标
;; (require 'saveplace)
;; (setq-default save-place t)

;; 使用M-up或者M-down 切换emacs窗口
(windmove-default-keybindings 'meta)

;; emacs 书签
;; C-x r m 设置书签
;; C-x r b 跳转书签
;; C-x r l 书签列表, d 删除, u 取消, r 重命名, x执行操作

;; 添加单词缩写
(setq-default abbrev-mode t)

;; 取色命令 gcolor2


;; 高亮修改过的行
;; (global-highlight-changes-mode)

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/magit")
;; (require 'magit)

(defun dir(&optional dirname)
  (interactive)
  (let* ((module (format "%s" dirname))
         (proj-dir (find2-shell-no-eof "git rev-parse --show-toplevel"))
        (module-dir (concat "/var/www/oa2/protected/modules/" module)))
    (if (file-exists-p module-dir)
        (cd module-dir)
      (if (string-match "protected/modules" proj-dir)
          (cd (find2-shell-no-eof (format "find \"%s\" -type d -name \"%s\" | head -1" proj-dir module)))
        ))))
(defalias 'eshell/d 'dir)


;; ctags -R -e --languages=php
;; M-. M-*
(setq tags-file-name "/var/www/oa2/TAGS")

;; 选中复制取消
(setq mouse-drag-copy-region nil)

;; git checkout . 还远当前文件夹
;; git cherry-pick 677e430 合并某个版本
;; git reset --hard HEAD
;; git reflog

;; 列模式 cua-mode
;; 合并取消 git merge --abort

;; 删除tag: git tag -d 2.1  git push origin :2.1

;; 查看某个版本的改变的信息
;; svn diff -r 版本号

;; 查看某个版本的日志所修改的文件
;; svn log -r 版本号 -v

;; 找出备份文件并删除 'find . -name "*~" -delete'
;; 文件多少行到多少行被删除  sed '1, 5d' -i oa.sql
;; 删除文件除某个外  rm -rf !(README)

;; 查找文件时忽略大小写
;; (setq read-buffer-completion-ignore-case t)
;; (setq read-file-name-completion-ignore-case t)

;; 追加提交
;; git add .
;; git ci --amend

;; 查找文件并替换
;; sed 's/\(filemode = \)true/\1false/' -i config

;; 开启部分提示
;; (partial-completion-mode 0)

;; 强制提交   git push -f

;; 列模式
(global-set-key [f6] 'cua-mode)

(defun kid-sdcv-to-buffer ()
  (interactive)
  (let ((word (if mark-active
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (current-word nil t))))
    (setq word (read-string (format "Search the dictionary for (default %s): " word)
                            nil nil word))
    (set-buffer (get-buffer-create "*sdcv*"))
    (buffer-disable-undo)
    (erase-buffer)
    (let ((process (start-process-shell-command "sdcv" "*sdcv*" "sdcv" "-n" word)))
      (set-process-sentinel
       process
       (lambda (process signal)
         (when (memq (process-status process) '(exit signal))
           (unless (string= (buffer-name) "*sdcv*")
             (setq kid-sdcv-window-configuration (current-window-configuration))
             (switch-to-buffer-other-window "*sdcv*")
             (local-set-key (kbd "d") 'kid-sdcv-to-buffer)
             (local-set-key (kbd "q") (lambda ()
                                        (interactive)
                                        (bury-buffer)
                                        (unless (null (cdr (window-list))) ; only one window
                                          (delete-window)))))
           (goto-char (point-min))))))))
(global-set-key (kbd "C-c d") 'kid-sdcv-to-buffer)


;; 编译定制
(setq my-compile-test-file "")
(setq my-compile-test-args "")
(setq compile-command "make -j 3  cd=true d=true")

(defun my-file-name (name)
  "获得文件名，返回结果不包括路径和扩展名"
  (setq fields (split-string name "/"))
  (setq name (elt fields (- (length fields) 1)))
  (setq len (length name))
  (setq i len)
  (setq ch "")
  (while (and (> i 0) (not (string= ch ".")))
    (progn
      (setq ch (substring name (- i 1) i))
      (setq i (- i 1))))
  (setq name (substring name 0 i)))

(setq my-compile-run-any-command "gcc")
(defun my-compile-run-any ()
  "编译任意的单个文件并运行编译好的程序"
  (interactive)
  (save-some-buffers t)
  (setq temp compile-command)
  (setq test-name nil)
  (setq type nil)
  (setq extension (file-name-extension buffer-file-name))
  (setq run-name (my-file-name buffer-file-name))
  (setq run-file (concat run-name "." extension))

  (setq command (concat my-compile-run-any-command " -o " run-name " " run-file))
  (setq command (concat command " && ./" run-name))
  (setq command (concat command " " my-compile-test-args))

  (compile command)
  (setq compile-command temp))
(defalias 'r 'my-compile-run-any)

;; 文件夹替换
;; 第一步：find-dired(按条件在文件夹下查找文件)
;; 第二步：标记要查找的文件 % m
;; 第三步：dired-do-query-replace-regexp 替换要查找的文件
;; 第四步：打开缓冲列表 保存替换的所有文件 * u s
