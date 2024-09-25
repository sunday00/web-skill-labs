local vim = vim
local Plug = vim.fn["plug#"]

vim.call("plug#begin", "~/.config/nvim/plugged")

--Plug ('nvim-neo-tree/neo-tree.nvim', { ['branch'] = 'v3.x' })
--    Plug 'nvim-lua/plenary.nvim'
--    Plug 'nvim-tree/nvim-web-devicons'
--    Plug 'MunifTanjim/nui.nvim'

Plug("preservim/nerdtree")
Plug("ryanoasis/vim-devicons")
Plug("Xuyuanp/nerdtree-git-plugin")

Plug("navarasu/onedark.nvim")

Plug("goolord/alpha-nvim")

Plug("nvim-lua/plenary.nvim")
Plug("nvim-telescope/telescope.nvim", { ["tag"] = "0.1.8" })
Plug("nvim-telescope/telescope-ui-select.nvim")

Plug("nvim-treesitter/nvim-treesitter", { ["do"] = ":TSUpdate" })

Plug("lukas-reineke/indent-blankline.nvim", { ["main"] = "ibl" })

Plug("numToStr/Comment.nvim")

Plug("nvim-lualine/lualine.nvim")
Plug("nvim-tree/nvim-web-devicons")

Plug("williamboman/mason.nvim")
Plug("williamboman/mason-lspconfig.nvim")
Plug("neovim/nvim-lspconfig")

Plug("m4xshen/autoclose.nvim")

Plug("neoclide/coc.nvim", { ["branch"] = "release" })

Plug("kevinhwang91/nvim-ufo")
Plug("kevinhwang91/promise-async")

Plug("stevearc/conform.nvim")

Plug("akinsho/toggleterm.nvim", { ["tag"] = "*" })

vim.call("plug#end")

require("config")
