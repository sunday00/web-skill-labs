require("mason").setup()
require("mason-lspconfig").setup({
	ensure_installed = {
		"lua_ls",
		"ts_ls",
		"eslint",
		"gopls",
		"intelephense",
		"rust_analyzer",
		"dockerls",
		"docker_compose_language_service",
	},
})

local lspconfig = require("lspconfig")
lspconfig.lua_ls.setup({})
lspconfig.ts_ls.setup({})
lspconfig.eslint.setup({})
lspconfig.gopls.setup({})
lspconfig.intelephense.setup({})
lspconfig.rust_analyzer.setup({})
lspconfig.dockerls.setup({})
lspconfig.docker_compose_language_service.setup({})

local KM = require("utils/keymapper").mapKey
KM("K", vim.lsp.buf.hover)
KM("gd", vim.lsp.buf.definition)
KM("<leader>ca", vim.lsp.buf.code_action)
