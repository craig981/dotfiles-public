set encoding=utf-8

set novb
set noerrorbells
set nocompatible
set nobackup
set noswapfile
set nohidden
if has("nvim") || v:version >= 800
	set nofixendofline
endif

set t_Co=256
set backspace=indent,start
set display=truncate
set sidescrolloff=5
set history=1000
set autoindent
set tabstop=8
set shiftwidth=8
set noexpandtab
set nowrap
set ruler
set pastetoggle=<F2>
set splitbelow splitright
" always show status line, not just in splits
set laststatus=2

set nohlsearch
" set incsearch

let mapleader=" "
nnoremap <leader>f :e <C-R>=expand('%:p:h') . '/'<CR>
nnoremap <leader>u :set invwrap<CR>
nnoremap <leader>t :term<CR>
nnoremap <leader>d :pwd<CR>
nnoremap <leader>D :lcd %:p:h <bar> pwd<CR>
nnoremap <leader>w :write<CR>
nnoremap <C-j> :b<space>
nnoremap <C-w>d :bd<CR>
nnoremap <silent> <C-w><C-d> :Bclose<CR>
"nnoremap <leader>i :30vs .<CR>
"nnoremap <leader>i :Vex <bar> :vertical resize 30<CR>
nnoremap <leader>i :set incsearch!<CR>
nnoremap <C-h> :set hlsearch!<CR>
nnoremap <leader>s :%s/\<<c-r><c-w>\>//gI<Left><Left><Left>
"toggle redraw during macro exec
nnoremap <leader>L :set invlazyredraw<CR>:set lazyredraw?<CR>

nnoremap <leader>l :setlocal spell!<CR>
set spelllang=en_gb

" remove multiple adjacent blank lines
vnoremap <leader>' :!cat -s
" align selection in a table
vnoremap <leader>= :!column -t<CR>gv
" remove trailing whitespace in selection
vnoremap <leader>\ :s/\s\+$//<CR>

" nnoremap <leader>r :silent grep '<c-r><c-w>' .<Left><Left><Left>
" if executable('ag')
" 	set grepprg=ag\ --vimgrep\ -s\ --ignore-dir\ .git\ --hidden\ $*
" 	set grepformat^=%f:%l:%c:%m
" endif

"occurrences in current file
nnoremap <leader>o :vimgrep /\<<c-r><c-w>\>/ %<CR>:cope<CR><C-w><C-p>

" search for all chars as normal text, e.g. paths with /
command! -nargs=1 SS let @/ = '\V'.escape(<q-args>, '\')
nnoremap <leader>/ :SS <C-R><C-A>

"format paragraph like emacs M-q
if has("nvim")
	nnoremap <A-q> gwip
else
	nnoremap <Esc>q gwip
endif

"command line emacs keys
cnoremap <c-n> <down>
cnoremap <c-p> <up>
cnoremap <c-f> <right>
cnoremap <c-b> <left>
cnoremap <c-a> <home>
cnoremap <c-e> <End>
"overrides show dir contents
"cnoremap <c-d> <Del>
if has("nvim")
	cnoremap <A-f> <S-Right>
	cnoremap <A-b> <S-Left>
	cnoremap <A-BS> <c-w>
	inoremap <A-BS> <c-w>
else
	cnoremap <Esc>f <S-Right>
	cnoremap <Esc>b <S-Left>
	cnoremap <Esc><Backspace> <c-w>
	inoremap <Esc><Backspace> <c-w>
endif
" recenter like emacs
nnoremap <C-l> zz
vnoremap <C-l> zz
" paste last deleted text
cnoremap <C-y> <C-r>"
inoremap <C-y> <C-r>"

nnoremap <C-p> <C-x>

" select last pasted text
nnoremap gp `[v`]

nnoremap Y yy

"delete to end of command line like emacs
function! KillLine()
	call setreg("\"", strpart(getcmdline(), getcmdpos() - 1))
	return strpart(getcmdline(), 0, getcmdpos() - 1)
endfunction
cnoremap <C-k> <C-\>eKillLine()<CR>

"make key below Esc on ISO keyboard same as US
noremap ± ~
noremap § `
noremap! ± ~
noremap! § `

" copy visual selection to clipboard
if has("mac") && has("clipboard")
	vnoremap Y "+y
endif
if has("linux")
	if !empty($SSH_CLIENT) || !empty($SSH_TTY)  " in ssh
		if has("clipboard") && has("X11")
			" don't connect to X server, like 'vim -X'
			set clipboard=exclude:.*
		endif
		if executable("xsel") && match($DISPLAY, "^:") == 0
			vnoremap Y :<C-u>silent '<,'>w !xsel -ib<CR>
		else
			vnoremap Y :<C-u>silent echoerr "Not yanking visual selection to clipboard of remote DISPLAY " . $DISPLAY<CR>
		endif
	elseif has("clipboard")
		vnoremap Y "+y
	elseif executable("xsel")
		vnoremap Y :<C-u>silent '<,'>w !xsel -ib<CR>
	else
		vnoremap Y :<C-u>silent echoerr "Can't yank visual selection to clipboard"<CR>
	endif
endif

if &diff
	nnoremap <leader>1 :diffget LOCAL<CR>
	nnoremap <leader>2 :diffget BASE<CR>
	nnoremap <leader>3 :diffget REMOTE<CR>
endif

nnoremap <C-n> :cn<CR>
nnoremap <C-p> :cp<CR>
nnoremap ]b :bn<CR>
nnoremap [b :bp<CR>

function! ToggleQuickFix()
	if getqflist({'winid' : 1}).winid
		cclose
	else
		copen
		wincmd p
	endif
endfunction
nnoremap <silent> <leader>q :call ToggleQuickFix()<CR>

augroup qf_below
	autocmd!
	autocmd FileType qf wincmd J
augroup END

set wildmenu
set wildmode=longest:full,full
set wildignore=**/build/**/*.h,*.o,*.a,*.so,*.pyc
set wildignore+=**/.git/*
if v:version >= 900
	set wildoptions=pum
endif

set tags=$HOME/dev/git/tags

"reload externally changed files
set autoread
au CursorHold * checktime

syntax on
set synmaxcol=200
set background=dark
hi StatusLine ctermbg=235 ctermfg=188

if filereadable(expand("~/.vim/autoload/plug.vim"))

	call plug#begin('~/.vim/plugged')
	Plug 'tpope/vim-commentary'
	Plug 'skywind3000/asyncrun.vim'
	Plug 'gruvbox-community/gruvbox'
	Plug 'adlawson/vim-sorcerer'
	Plug 'mswift42/vim-themes'
	Plug 'arcticicestudio/nord-vim'
	Plug 'ctrlpvim/ctrlp.vim'
	Plug 'wincent/ferret'
	call plug#end()

	augroup change_the_colours
		autocmd!
		"Switch off bright green colouring of -- INSERT --
		autocmd ColorScheme sorcerer hi clear ModeMsg
		"Thinner vertical split
		autocmd ColorScheme sorcerer hi VertSplit cterm=NONE ctermbg=NONE
		autocmd ColorScheme sorcerer hi Visual cterm=NONE ctermbg=23 ctermfg=15
		autocmd ColorScheme sorcerer hi IncSearch ctermbg=236 ctermfg=7 cterm=NONE
		autocmd ColorScheme reykjavik hi Identifier gui=NONE
		autocmd ColorScheme reykjavik hi Visual guifg='#dddddd' guibg='#116677'
		autocmd ColorScheme reykjavik hi WarningMsg guifg='#ffffff'
		autocmd ColorScheme reykjavik hi IncSearch guifg='#ffffff' guibg=NONE
		autocmd ColorScheme reykjavik hi CurSearch guifg='#000000' guibg='#ffffff'
		autocmd ColorScheme reykjavik hi Search guibg='#909090' guifg='#000000'
	augroup END

	"autocmd VimEnter * colorscheme gruvbox
	"colorscheme gruvbox
	"colorscheme desert
	if has("termguicolors")
		set termguicolors
		if !has("nvim")
			set term=xterm-256color
		endif
		" colorscheme reykjavik
		colorscheme nord
	else
		colorscheme sorcerer
	endif
	hi clear SpellBad
	hi SpellBad guifg=white guibg=red

	nnoremap ,m :cope <bar> AsyncRun make -f build.mk run<CR><C-W><C-P>
	nnoremap <C-C><C-K> :AsyncStop<CR>

	if match(&runtimepath, 'ctrlp.vim') != -1
		let g:ctrlp_map = ''
		let g:ctrlp_show_hidden = 1
		let g:ctrlp_use_caching = 0
		let g:ctrlp_clear_cache_on_exit = 1
		let g:ctrlp_prompt_mappings = {
					\ 'PrtSelectMove("j")':   ['<c-n>', '<down>'],
					\ 'PrtSelectMove("k")':   ['<c-p>', '<up>'],
					\ 'PrtHistory(-1)':       ['<c-j>'],
					\ 'PrtHistory(1)':        ['<c-k>'],
					\ }
		nnoremap <silent> <leader>e :CtrlP getcwd()<CR>
	else
		nnoremap <leader>e :find<space>
	endif

	nnoremap <leader>r :Ack <c-r><c-w> -w
	let g:FerretHlsearch=1
	let g:FerretExecutable='ag,rg'
	let g:FerretExecutableArguments = {
				\ 'ag': '--vimgrep -s --ignore-dir .git --hidden'
				\ }
endif

if has("nvim")
	nnoremap <A-o> <C-W><C-W>
	tnoremap <A-o> <C-\><C-n><C-W><C-W>
	tnoremap <Esc> <C-\><C-n>
	set guicursor=n-v-c-sm-i-ci-ve:block,r-cr-o:hor20

	augroup term_group
		autocmd!
		" insert mode by default
		autocmd TermOpen term://* startinsert
	augroup END

else
	nnoremap <Esc>o <C-W><C-W>
	if v:version >= 801
		tnoremap <Esc>o <C-\><C-n><C-W><C-W>
		tnoremap <C-W><C-Y> <C-W>""
	endif

	" Ctrl-[ quicker
	set ttimeoutlen=50

	"remove comment when joining lines
	if v:version > 703
		set formatoptions+=j
	endif
endif
" stop adding comment when opening a new line
autocmd FileType,BufNewFile,BufRead * set formatoptions-=o

" tcsh startup slow at work
if &shell == "/bin/tcsh"
	set shell=/bin/tcsh\ -f
endif

let g:netrw_banner=0
let g:netrw_browse_split=4
let g:netrw_altv=1		" open splits to the right
let g:netrw_liststyle=3		" tree




" indent the wrapped region. cursor ends up on the first line.
function! s:WrapIndentRegion(vis)
	execute "silent normal! " . (a:vis ? "'<>'>" : ">ap")
endfunction

" jump to the last line of the wrapped region
function! s:WrapEndRegion(vis)
	execute "normal! " . (a:vis ? "'>" : "}k")
endfunction

" wrap paragraph or visual selection in #if 0
function! WrapPreprocessIf(vis)
	execute "silent normal! " . (a:vis ? "'<" : "{j")
	execute "normal! O#if 1\<Esc>"
	execute "normal! " . (a:vis ? "'>" : "}k")
	execute "normal! o#endif\<Esc>F#%"
endfunction

" wrap paragraph or visual selection in a block
function! WrapBlock(vis)
	call <SID>WrapIndentRegion(a:vis)
	execute "normal! O\<C-d>{\<Esc>=="
	call <SID>WrapEndRegion(a:vis)
	execute "normal! o\<C-d>}\<Esc>==%"
endfunction

" add MEL curve point "-p x y z"
function! InsertCurvePoint()
	call inputsave()
	let var = input('var: ')
	call inputrestore()
	execute "normal! o\"-p \" << " . var . "[0] << \" \" << " . var . "[1] << \" \" << " . var . "[2] << \" \"\<Esc>"
endfunction


function! CppMode()
	" c-u wipes out visual range on command line
	nnoremap <buffer> <leader>b :call WrapBlock(0)<CR>
	vnoremap <buffer> <leader>b :<c-u>call WrapBlock(1)<CR>
	nnoremap <buffer> <leader>3 :call WrapPreprocessIf(0)<CR>
	vnoremap <buffer> <leader>3 :<c-u>call WrapPreprocessIf(1)<CR>

	nnoremap <buffer> <leader>p :call InsertCurvePoint()<CR>

	setlocal tw=0
	"make vim-commentary use C++ style comments
	setlocal commentstring=//\ %s

	"highlight text over 80 column limit
	"highlight OverLength ctermfg=green guifg=#29AA29
	"match OverLength /\%81v.\+/

	"stop forcing preprocessor directives to column 1
	setlocal cinkeys-=0#
endfunction

function! TxtMode()
	setlocal tw=70
endfunction

function! ArnoldMode()
	setlocal ts=2
	setlocal sw=2
	setlocal expandtab
	setlocal commentstring=#\ %s
endfunction


augroup filetype_cpp
	autocmd!
	autocmd FileType cpp call CppMode()
	autocmd BufNewFile,BufRead *.cpp,*.cc,*.h,*.c call CppMode()
augroup END

augroup filetype_txt
	autocmd!
	autocmd FileType txt call TxtMode()
	autocmd FileType md call TxtMode()
	autocmd BufNewFile,BufRead *.txt,*.md call TxtMode()
augroup END

augroup filetype_arnold
	autocmd!
	autocmd BufNewFile,BufRead *.ass call ArnoldMode()
augroup END


":help vimrc-filetype
filetype plugin indent on

":help find-manpage
runtime! ftplugin/man.vim
nnoremap K :Man <C-R><C-W><CR>


set path=.,**
set path+=**/.[a-z]*/**

let $MY_DOTFILES_VIMRC = $HOME . "/dotfiles/vimrc"
if filereadable($MY_DOTFILES_VIMRC)
    source $MY_DOTFILES_VIMRC
endif
