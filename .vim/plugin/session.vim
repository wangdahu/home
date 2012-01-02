"          File: session.vim
"   Description: Open and Save vim sessions
"        Author: <uedsky at gmail dot com>
" Last Modified: Jun 30, 2011
"       Version: 1.0

if exists("loaded_session") || &cp
    finish
endif
let loaded_session = 1

if !exists('g:session_autosave')
    let g:session_autosave = 1
endif

if !exists('g:sessoin_path')
    let g:session_path = $HOME . (isdirectory($HOME . '/.vim') ? '/.vim' : '/vimfiles') . '/cache'
    if !isdirectory(g:session_path) | call mkdir(g:session_path, 'p') | endif
endif

if !exists('g:session_maxcount')
    let g:session_maxcount = 5
endif

autocmd VimLeave * call s:savewin()
function! s:savewin()
    let files = split(globpath(g:session_path, '_????????.session'), '\n')
    while len(files) >= g:session_maxcount
        call delete(remove(files, 0))
    endwh
    if g:session_autosave && !s:is_noname()
        call s:save_session('')
    endif
endfunction

" return 1 if the buffers only '[No Name]'
function! s:is_noname()
    redir => bufstr
        buffers
    redir END
    return len(split(bufstr, '\n')) == 1 && bufname('') == ''
endfunction

function s:save_session(session_name)
    if a:session_name != ''
        let g:session_name = a:session_name
    endif
    if !exists("g:session_name")
        let g:session_name = strftime("_%d%H%M%S")
    endif
    let session_path = s:name_to_path(g:session_name)
    execute 'mksession! '.session_path
    let lines = readfile(session_path)
    call insert(lines, "let g:session_name = '". g:session_name ."'", - 1)
    call writefile(lines, session_path)
endfunction

function s:open_session(session_name)
    let session_path = s:name_to_path(a:session_name)
    if a:session_name == '' " if name is omitted, open the lastest one
        let session_files = split(globpath(g:session_path, '*.session'), '\n')
        let session_times = []
        for filename in session_files
            call add(session_times, getftime(filename))
        endfor
        let latest_index = index(session_times, max(session_times))
        let session_path = session_files[latest_index]
    endif
    execute 'so' session_path
endfunction

function s:name_to_path(name)
    return g:session_path. '/' .a:name.'.session'
endfunction

function s:path_to_name(path)
  return fnamemodify(a:path, ':t:r')
endfunction

function s:compare_ftime(name1, name2)
     return getftime(s:name_to_path(a:name1)) < getftime(s:name_to_path(a:name2))
endfunction

function s:complete_names(arg, line, pos)
    let session_files = split(globpath(g:session_path, '*.session'), '\n')
    let session_names = map(session_files, 's:path_to_name(v:val)')

    let names = filter(session_names, 'v:val =~ a:arg')

    let unamed_files = []
    let named_files = []
    for name in names
        if name =~ '^_\d\{8}'
            call add(unamed_files, name)
        else
            call add(named_files, name)
        endif
    endfor
    call reverse(unamed_files) " reverse 'unamed' names
    call sort(named_files, function("s:compare_ftime"))
    let names = extend(named_files, unamed_files)

    return map(names, 'fnameescape(v:val)')
endfunction

command! -nargs=? -complete=customlist,s:complete_names SaveSession call s:save_session(<q-args>)
command! -nargs=? -complete=customlist,s:complete_names OpenSession call s:open_session(<q-args>)
