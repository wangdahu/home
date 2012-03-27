
let s:isWin = has("win32") || has("win64")
let $VIMFILES = $HOME . "/.vim"
let $CACHEDIR = $VIMFILES . "/cache"

set nocompatible            "Use Vim settings, rather than Vi settings (much better!).

set fileencodings=ucs-bom,utf-8,cp936,gb18030,big5,euc-jp,euc-kr,latin1
set encoding=utf-8
set fileformats=unix,dos
syntax on
filetype plugin indent on
set number
set tabstop=4
set expandtab
set shiftwidth=4            " << , >>
set softtabstop=4           " backspace
set shiftround
" set smartindent
" set foldmethod=indent
set backspace=indent,eol,start  " allow backspacing over everything in insert mode
set whichwrap+=<,>,[,]
" set cmdheight=2
" set list                    " show tab characters.Visual Whitespace.
set listchars=eol:¶,tab:>-,trail:·,extends:»,precedes:« " 182, , 187, 171
set scrolloff=1
set ignorecase smartcase    " :set noic
"set hidden
set nobackup                " whether keep a backup file
set backupcopy=yes          " do not change permision in windows
set autowrite
set autochdir               " auto change current working directory whenever you open a file
set history=50              " keep 50 lines of command line history
set ruler                   " show the cursor position all the time
set showcmd                 " display incomplete commands
set hlsearch
set incsearch               " do incremental searching
set nostartofline
set nojoinspaces            " do not insert two spaces after [.?!] when join lines
set complete=.,b,d,i,k,t,u,w
set diffopt=vertical
set splitright splitbelow
set sessionoptions=buffers,curdir,folds,resize,tabpages " localoptions
set iskeyword=@,48-57,_,-
" set spell
set display=lastline
set path=,,~/,~/Documents
set wildmode=longest:full
set wildignore=*.swp,.DS_Store,.localized,.git,.svn
set wildmenu
autocmd BufRead * if &ff != 'unix' || &fenc != 'utf-8' | setl laststatus=2 | endif
set statusline=%f\ %M[%{&ff},%{&fenc}]%y%r%=%l,%c\ \ \ %P
set undodir=$CACHEDIR
set undofile
set dir=$CACHEDIR//
set winaltkeys=no
set modelines=2
if has("gui_running")
    set lines=30                " Vim window size
    set columns=100
    set guioptions-=T           " hide tool bar
    set guioptions-=m           " hide menu bar
    autocmd Filetype javascript setlocal makeprg=jsl\ -nologo\ -nofilelisting\ -nosummary\ -nocontext\ -conf\ $VIMFILES/vimbin/jsl.conf\ -process\ %
    autocmd BufWritePost *.js call Make()
endif

let mapleader=';'           " define before key mappings
" map-modes map-listing {{{
" Use Ctrl-o instead of <Esc> in insert mode mappings
" move cursor in insert mode
imap <M-h> <Left>
imap <M-j> <C-o><Down>
imap <M-k> <C-o><Up>
imap <M-l> <Right>
imap <M-e> <C-o>A
imap <M-a> <C-o>I
imap <M-d> <C-o>dw
imap <M-Backspace> <C-o>db  " diff from <C-w>
imap <M-o> <C-o>O

nmap <Esc><Esc> :nohl<CR>
" move cursor in long line
map <Up> gk
map <Down> gj

nnoremap Y y$
vnoremap <BS> d             " backspace in Visual mode deletes selection
cnoremap <C-a> <Home>
cnoremap <C-f> <Right>
cnoremap <C-b> <Left>
cnoremap <M-f> <S-Right>
cnoremap <M-b> <S-Left>

" search for visually selected text
function! s:search(type)
    let old_reg = getreg('"')
    let old_regtype = getregtype('"')
    normal gvy
    let @/ = substitute(escape(@", a:type . '.\*$^~['), '\_s\+', '\\_s\\+', 'g')
    call setreg('"', old_reg, old_regtype)
endfunction
vnoremap * :<C-u>call <SID>search('/')<CR>/<C-r>/<CR>
vnoremap # :<C-u>call <SID>search('?')<CR>?<C-r>/<CR>

function! Make()
    if &modified | silent write | endif
    if &makeprg == 'make' | return | endif
    silent make
    execute 'cw'
endfunction
map <F12> :call <SID>run_script()<CR>
imap <F12> <Esc>:call <SID>run_script()<CR>
if !exists("*s:run_script")
function! s:run_script()
    call Make()

    let sys_prefix = s:isWin ? '!' : '!./'
    let fname = shellescape(expand("%:t"))
    if index(["python", "sh", "dosbatch"], &ft) > -1
        execute sys_prefix . fname
    elseif &ft == "vim"
        execute "source %"
    elseif index(["c", "cpp"], &ft) > -1
        if !len(getqflist())
            execute sys_prefix . expand("%:t:r")
        endif
    elseif &ft == "php"
        let cmd =  (exists('g:php_command') ? g:php_command : 'php') . ' ' . fname
        let output = system(cmd)
        if len(output) | echo output | endif
        " execute '!' . cmd . ' ' . fname
    elseif index(["html", "markdown", "vb"], &ft) > -1
        try
            python import webbrowser,vim; webbrowser.open('file://' + vim.current.buffer.name)
        catch /.*/
            silent execute '!' . fname
        endtry
    endif
endfunction
endif

" end mappings }}}

" autocommands. {{{
augroup filetype_config
    autocmd!

    autocmd BufRead,BufNewFile *.json   set filetype=javascript
    autocmd BufRead,BufNewFile *.less   set filetype=less       syntax=less

    autocmd Filetype javascript setlocal dictionary=$VIMFILES/dict/javascript.dict

    autocmd FileType css setlocal dictionary=$VIMFILES/dict/css.dict |
                \ setlocal iskeyword+=#

    autocmd FileType php setlocal iskeyword+=$

    autocmd FileType html,php,jsp,javascript setl includeexpr=substitute(v:fname,'^/','','')

    let $PRG_EXT = s:isWin ? ".exe" : ""
    autocmd FileType c setlocal makeprg=gcc\ -o\ %<$PRG_EXT\ %
    autocmd FileType cpp setlocal makeprg=g++\ -o\ %<$PRG_EXT\ %

    autocmd FileType vim,help setlocal keywordprg=:help

    " When editing a file, always jump to the last known cursor position.
    " And open enough folds to make the cursor is not folded
    " Don't do it when the position is invalid or when inside an event handler
    " (happens when dropping a file on gvim).
    autocmd BufWinEnter *
                \ if line("'\"") <= line("$") |
                \   exe "normal! g`\"" | exe "normal! zv" |
                \ endif

    " Automating read-only access to existing files
    autocmd SwapExists * let v:swapchoice = 'o'

augroup END " }}}

" Compare current buffer and the file it was loaded from in vertical split
" window
command! DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
            \| wincmd p | diffthis

command! W exe 'silent write !sudo tee %'

" locate file in Finder {{{
function! s:open(path)
    let path = shellescape(a:path != '' ? a:path : expand('%'))
    let cmd = 'open -R '
    if executable('explorer')
       let cmd = 'explorer /select,'
    elseif executable('nautilus') " ubuntu
       let cmd = 'nautilus -n '
    endif
    call system(cmd . path)
endfunction
command! -nargs=? -complete=file Open call s:open(<q-args>)
" }}}

" plugin config {{{

" html indent
let g:html_exclude_tags = ['html', 'style', 'script', 'body']

" ToHtml
let html_use_css = 0
let html_use_encoding = 'UTF-8'
let html_number_lines = 1

" taglist-intro
let Tlist_Auto_Open = 0
let Tlist_Use_Right_Window = 1
let Tlist_Exit_OnlyWindow = 1
let Tlist_File_Fold_Auto_Close = 1
" let Tlist_Show_One_File = 1
let tlist_php_settings = 'php;c:class;d:constant;f:function'
" let Tlist_Compact_Format = 1
map <F4> :TlistToggle<CR>
imap <F4> <C-o>:TlistToggle<CR>

" NERDTree
" NERDTreeGlobalCommands
" NERDTreeMappings
map <F3> :NERDTreeToggle<CR>
imap <F3> <C-o>:NERDTreeToggle<CR>

" NERDCommenter
" <leader>cy    NERDComYankComment
" <leader>cs    NERDComSexyComment
" <leader>c$    NERDComEOLComment
" <leader>cA    NERDComAppendComment
" <leader>ca    NERDComAltDelim
let NERDMenuMode = 0
let NERDSpaceDelims = 1
imap <M-/> <C-o><Plug>NERDCommenterToggle

" zencoding
" <C-Y>n        zencoding-goto-next-point
" <C-Y>k        zencoding-remove-tag
" <C-Y>j        zencoding-split-join-tag
" <C-Y>a        zencoding-make-anchor-url
let user_zen_settings = {'indentation' : '    '}
" let use_zen_complete_tag = 1

" :Grep
" :Egrep
" :Fgrep
" :Agrep
" :Rgrep
" :Regrep
" :Rfgrep
" :Ragrep
" :Bgrep
" :GrepBuffer
" :GrepArgs
" let Grep_Default_Filelist   = '*.php *.phtml *.js *.html *.css'
let Grep_Default_Filelist   = '*.js'
" let Grep_Default_Options    = '-i'
let Grep_Skip_Dirs          = '.git .svn images'
let Grep_Skip_Files         = '*~ *.swp'
" let Grep_OpenQuickfixWindow = 0
" let Grep_Find_Use_Xargs     = 0
" let Grep_Xargs_Options      = '--print0'
" let Grep_Cygwin_Find        = 1
" let Grep_Null_Device        = '/dev/null'
" let Grep_Shell_Quote_Char   = "'"
" let Grep_Shell_Escape_Char  = "'"
" }}}

if s:isWin
    let &rtp = substitute(&rtp, 'vimfiles', '.vim', 'g')
    set termencoding=cp936  " for windows console
    language messages zh_CN.utf-8
endif

if filereadable(expand('~/.vimrc.local'))
    source ~/.vimrc.local
endif

" vim: ft=vim fdm=marker
