" Single file vimrc, its meant to stay short and simple.
set nocompatible

syntax on                  " Enable syntax highlighting.

" set relativenumber number " Show relative line numbers
set nowrapscan " Dont wrap search

set backspace   =indent,eol,start  " Make backspace work as you would expect.
set hidden                 " Switch between buffers without having to save first.
set laststatus  =2         " Always show statusline.
set display     =lastline  " Show as much as possible of the last line.

set showmode               " Show current mode in command-line.
set showcmd                " Show already typed keys when more are expected.

set incsearch              " Highlight while searching with / or ?.
set hlsearch               " Keep matches highlighted.
set ignorecase " case insensitive
set smartcase  " use case if any caps used

" You should use .editorconfig, but in case there isnt one
set shiftwidth=2
set tabstop=2
set expandtab

" Our colorscheme, optional obviously
" colorscheme darkblue

" Use the OS clipboard by default
set clipboard+=unnamedplus

" Changes the cwd to the directory of the current buffer whenever you switch buffers.
" set autochdir
" Make the file browser always open the current directory.
" set browsedir=current
" let g:netrw_keepdir=0

let s:cache_dir = '~/.vim/cache_vim'
if has('nvim')
  let s:cache_dir = '~/.vim/cache_nvim'
endif

function! s:get_cache_path(suffix)
  return resolve(expand(s:cache_dir . '/' . a:suffix))
endfunction

function! EnsureExists(path)
  if !isdirectory(expand(a:path))
    call mkdir(expand(a:path))
  endif
endfunction

if exists('+undofile')
  set undofile
  let &undodir = s:get_cache_path('undo')
endif

" backups
set backup
let &backupdir = s:get_cache_path('backup')
set backupext   =-vimbackup
set backupskip  =

" swap files
let &directory = s:get_cache_path('swap')
set noswapfile
set updatecount =100

" viminfo stores information such as the command history, buffer list, global variables, ...
let viminfopath = s:get_cache_path('viminfo')
let &viminfo = '''1000,n' . viminfopath

call EnsureExists(s:cache_dir)
call EnsureExists(&undodir)
call EnsureExists(&backupdir)
call EnsureExists(&directory)

" cf: https://github.com/junegunn/vim-plug/wiki/tips#automatic-installation
" To reinstall the plugins when the list changes: rm ~/.vim/autoload/plug.vim
let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin()
Plug 'junegunn/fzf.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/vim-peekaboo' " Preview the registers before they are used
Plug 'tpope/vim-dispatch'
Plug 'editorconfig/editorconfig-vim'
call plug#end()

" Minimal keymap
" Mostly mapping for fzf: https://github.com/junegunn/fzf.vim?tab=readme-ov-file#commands
let mapleader = "\,"
" Open files in git repo
nnoremap <Leader>g :GFiles<CR>
" Open files in pwd
nnoremap <Leader>f :Files<CR>
" Currently and previously opened files.
nnoremap <Leader>b :History<CR>
" Open currently opened files
nnoremap <Leader>B :Buffers<CR>
" Last command typed
nnoremap <Leader>, :History:<CR>
" List all commands
nnoremap <Leader>p :Commands<CR>
" Ripgrep in pwd
nnoremap <Leader>j :Rg<CR>
" Search in the current buffer
nnoremap <Leader>s :BLines<CR>
" Search in all opened buffers
nnoremap <Leader>S :Lines<CR>
" Clear search highlights when in normal mode
" Use <Enter> instead of <Esc> because <Esc> has the side of making vim
" behave incorrectly when running in tmux in Ubuntu 22.
nnoremap <silent> <Enter> :noh<CR>
" Remap for destroying trailing whitespace cleanly and then writting to disk.
:nnoremap <Leader>w :let _save_pos=getpos(".") <Bar>
    \ :let _s=@/ <Bar>
    \ :%s/\s\+$//e <Bar>
    \ :let @/=_s <Bar>
    \ :nohl <Bar>
    \ :unlet _s<Bar>
    \ :call setpos('.', _save_pos)<Bar>
    \ :unlet _save_pos<CR><CR>
    \ :write<CR>
" Quickfix list
nnoremap <Leader>1 :cnext<CR>
nnoremap <Leader>2 :cprev<CR>
