local vim = vim
local Plug = vim.fn['plug#']

vim.call('plug#begin', '~/.config/nvim/plugged')

Plug ('nvim-neo-tree/neo-tree.nvim', { ['branch'] = 'v3.x' })
    Plug 'nvim-lua/plenary.nvim'
    Plug 'nvim-tree/nvim-web-devicons'
    Plug 'MunifTanjim/nui.nvim'

Plug 'navarasu/onedark.nvim'

Plug 'nvim-lua/plenary.nvim'
Plug ('nvim-telescope/telescope.nvim', { ['tag'] = '0.1.8' })

vim.call('plug#end')



require("config")
