set encoding=utf-8

"stop annoying flashing of screen
set novb
set noerrorbells

set nocompatible
set nobackup
set noswapfile

set t_Co=256
set backspace=indent,start
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
"set incsearch
nnoremap <C-l> :set invhlsearch<CR>

nnoremap j gj
nnoremap k gk
nnoremap gj j
nnoremap gk k

nnoremap <C-p> <C-x>

nnoremap Y yy

"format paragraph like emacs M-q
if has("nvim")
	nnoremap <A-q> gwip
else
	nnoremap <Esc>q gwip
endif

let mapleader=" "
nnoremap <leader>f :e <C-R>=expand('%:p:h') . '/'<CR>
nnoremap <leader>h :sf<space>
nnoremap <leader>u :set invwrap<CR>
nnoremap <leader>j :b<space>
nnoremap <leader>t :term<CR>
nnoremap <leader>d :pwd<CR>
nnoremap <leader>D :lcd %:p:h <bar> pwd<CR>
nnoremap <leader>w :write<CR>
nnoremap <C-w>d :bd<CR>
nnoremap <silent> <C-w><C-d> :Bclose<CR>
"nnoremap <leader>i :30vs .<CR>
nnoremap <leader>i :Vex <bar> :vertical resize 30<CR>
nnoremap <leader>z :e ~/.vimrc<CR>
nnoremap <leader>s :%s/\<<c-r><c-w>\>//gI<Left><Left><Left>
"toggle redraw during macro exec
nnoremap <leader>L :set invlazyredraw<CR>:set lazyredraw?<CR>
nmap <leader>; gcc

"align selection in a table
vnoremap <leader>= :!column -t<CR>gv
" remove trailing whitespace in selection
vnoremap <leader>\ :s/\s\+$//<CR>

nnoremap <leader>r :silent grep '<c-r><c-w>' .<Left><Left><Left>
if executable('ag')
	set grepprg=ag\ --vimgrep\ -s\ --hidden\ $*
	set grepformat^=%f:%l:%c:%m
endif

"occurrences in current file
nnoremap <leader>o :vimgrep /\<<c-r><c-w>\>/ %<CR>:cope<CR><C-w><C-p>

" search for all chars as normal text, e.g. paths with /
command! -nargs=1 SS let @/ = '\V'.escape(<q-args>, '\')
nnoremap <leader>/ :SS <C-R><C-A>

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

"paste last deleted text
cnoremap <C-y> <C-r>"
inoremap <C-y> <C-r>"

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

if has("clipboard")
	vnoremap Y "+y
elseif has("linux")
	" copy visual selection to clipboard
	vnoremap Y :<C-u>silent '<,'>w !xsel -ib<CR>
endif

if &diff
	nnoremap <leader>1 :diffget LOCAL<CR>
	nnoremap <leader>2 :diffget BASE<CR>
	nnoremap <leader>3 :diffget REMOTE<CR>
else
	"next/previous in quickfix/buffer list
	nnoremap <C-j> :cn<CR>
	nnoremap <C-k> :cp<CR>
	nnoremap ]b :bn<CR>
	nnoremap [b :bp<CR>
endif

function! ToggleQuickFix()
	if getqflist({'winid' : 1}).winid
		cclose
	else
		copen
		wincmd p
	endif
endfunction
nnoremap <silent> Q :call ToggleQuickFix()<CR>

augroup qf_below
	autocmd!
	autocmd FileType qf wincmd J
augroup END

set wildmenu
set wildmode=longest:full,full
set wildignore+=**/build/**/*.h,*.o,*.a,*.so,*.pyc

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
	if has("nvim")
		Plug 'nvim-lua/plenary.nvim'
		Plug 'nvim-telescope/telescope.nvim'
	else
		" Plug 'machakann/vim-highlightedyank'
	endif
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
	augroup END

	"autocmd VimEnter * colorscheme gruvbox
	"colorscheme gruvbox
	"colorscheme desert
	if has("termguicolors")
		set termguicolors
		set term=xterm-256color
		colorscheme reykjavik
	else
		colorscheme sorcerer
	endif

	if has("nvim")
		au TextYankPost * silent! lua vim.highlight.on_yank {timeout=300}
	else
		" let g:highlightedyank_highlight_duration = 300
	endif

	nnoremap ,m :cope <bar> AsyncRun make -f build.mk run<CR><C-W><C-P>
	nnoremap <C-C><C-K> :AsyncStop<CR>

	if has("nvim")
		lua << EOF
		local telescope = require('telescope')
		telescope.setup {
			pickers = {
				find_files = {
					file_ignore_patterns = {".git/"},
					hidden = true
				}
			}
		}
EOF
		nnoremap <leader>e <cmd>Telescope find_files<cr>

	elseif executable('fzy')
		function! FzyCommand(choice_command, vim_command)
			try
				let output = system(a:choice_command . " | fzy ")
			catch /Vim:Interrupt/
				" Swallow errors from ^C, allow redraw! below
			endtry
			redraw!
			if v:shell_error == 0 && !empty(output)
				exec a:vim_command . ' ' . output
			endif
		endfunction
		nnoremap <leader>e :call FzyCommand("find . \\( -type d -name .git \\) -prune -false -o -type f", ":e")<cr>
	else
		nnoremap <leader>e :find<space>
	endif
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
	endif

	" Ctrl-[ quicker
	set ttimeoutlen=50

	"remove comment when joining lines
	if v:version > 703
		set formatoptions+=j
	endif
endif

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
