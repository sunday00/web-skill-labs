# setting

## pre install
- install neovim
- install vim-plug
  - https://github.com/junegunn/vim-plug
  - https://junegunn.github.io/vim-plug/example-lua/
- install ripgrep
  - brew install ripgrep
  - rg -V

## path
- `/nvim` folder to `~/.config/nvim`


# helped ref
- https://velog.io/@mythos/Linux-neovim-%EC%84%A4%EC%A0%95-CoC-Vim-Plug-treesitter-NERDTree
- https://www.youtube.com/watch?v=M8YrfquecWc


# update
- coc: autocomplete lsp
    - https://github.com/neoclide/coc.nvim/wiki/Using-coc-extensions

- lsp: https://github.com/williamboman/mason-lspconfig.nvim

# prequently keymap / command
- <leader> : space
- <C > : ctrl
- <Sft >: shift
  - officially document not imply Sht. 
  - usually say just capitalized.

## file search
- <leader e>: open tree
- <leader ff>: open file search

## undo/redo
- u
- <C r> 

## comment
- <C g><C g>

## tab
- :tabnew
- gt: next tab
- gT: prev tab

## split
- :vert new
- :split new
- <C w> + <Sht H|J|K|L>: flip split screen

## terminal
- :ToggleTerminal
- <C \><C n> : terminal to normal
