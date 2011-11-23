"         Name: git-rev.vim v1.0
"       Author: <uedsky at gmail dot com>
" Last Modifed: Nov 23, 2011
"  Description: diff or open git revision files
" You can do these
"   :diffspit HEAD:%
" or
"   $ vim HEAD~10:file
" or
"   :e branch:file
" Based on http://www.vim.org/scripts/script.php?script_id=2185

function s:LoadRev()
    let folder = expand("%:p:h")
    let file = expand("%:p")
    while ! isdirectory(folder . "/.git") && folder != '/'
        let folder = fnamemodify(folder, ":p:h:h")
    endwhile
    let file = substitute(file, '^'. folder .'/', '', '')
    let file = substitute(file, '^\(.*/\)\([^:]*:\)\(.*\)', '\2\1\3', '')

    if ! filereadable(file) && line('$')==1
        let cmd = "git show " . file
        exec 'normal :r!LANG=C ' . cmd . "\n:1d\n"

        setl readonly nomodifiable
    endif

endfunction

" command -nargs=? GitDiff call s:OpenDiff(<q-args>)
autocmd BufNewFile *:* call s:LoadRev()
map <leader>gf :diffsplit :%<CR><C-w><C-w>gg]c
map <leader>gg :diffsplit HEAD:%<CR><C-w><C-w>gg]c
map <leader>gh :diffsplit HEAD^:%<CR><C-w><C-w>gg]c
