local os_info = os.getenv('HOME')
print(os_info)

vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Displays which-key popup sooner
vim.opt.timeoutlen = 300
vim.opt.shortmess:append("I")

local FoldMethods = {
  indent = 'indent',
  syntax = 'syntax',
  marker = 'marker',
  expr = 'expr',
  manual = 'manual',
}

-- :help options
vim.opt.backup = false                          -- creates a backup file
vim.opt.clipboard = "unnamedplus"               -- allows neovim to access the system clipboard
vim.opt.cmdheight = 1                           -- more space in the neovim command line for displaying messages
vim.opt.completeopt = { "menuone", "noselect" } -- mostly just for cmp
vim.opt.conceallevel = 0                        -- so that `` is visible in markdown files
vim.opt.colorcolumn = "140"
vim.opt.fileencoding = "utf-8"                  -- the encoding written to a file
vim.opt.hlsearch = true                         -- highlight all matches on previous search pattern
vim.opt.ignorecase = true                       -- ignore case in search patterns
vim.opt.mouse = "a"                             -- allow the mouse to be used in neovim
vim.opt.pumheight = 10                          -- pop up menu height
vim.opt.showmatch = true
vim.opt.showmode = false                        -- we don't need to see things like -- INSERT -- anymore
vim.opt.showtabline = 2                         -- always show tabs
vim.opt.smartcase = true                        -- smart case
vim.opt.smartindent = true                      -- make indenting smarter again
vim.opt.splitbelow = true                       -- force all horizontal splits to go below current window
vim.opt.splitright = true                       -- force all vertical splits to go to the right of current window
vim.opt.swapfile = false                        -- creates a swapfile
vim.opt.termguicolors = true                    -- set term gui colors (most terminals support this)
vim.opt.timeoutlen = 100                        -- time to wait for a mapped sequence to complete (in milliseconds)
vim.opt.undofile = true                         -- enable persistent undo
vim.opt.updatetime = 300                        -- faster completion (4000ms default)
vim.opt.writebackup = false                     -- if a file is being edited by another program (or was written to file while editing with another program), it is not allowed to be edited
vim.opt.expandtab = true                        -- convert tabs to spaces
vim.opt.shiftwidth = 2                          -- the number of spaces inserted for each indentation
vim.opt.tabstop = 2                             -- insert 2 spaces for a tab
vim.opt.cursorline = true                       -- highlight the current line
vim.opt.number = true                           -- set numbered lines
vim.opt.relativenumber = true                   -- set relative numbered lines
vim.opt.numberwidth = 2                         -- set number column width to 2 {default 4}
vim.opt.signcolumn = "yes"                      -- always show the sign column, otherwise it would shift the text each time
vim.opt.wrap = false                            -- display lines as one long line
vim.opt.scrolloff = 8                           -- is one of my fav
vim.opt.sidescrolloff = 8
vim.opt.guifont = "monospace:h17"               -- the font used in graphical neovim applications
vim.opt.foldenable = true                       -- enables folding
vim.opt.foldmethod = FoldMethods.manual         -- set default folding method
-- nofoldenable = false                         -- disable folding
vim.opt.shortmess:append "c"
vim.opt.winborder = 'rounded'

vim.opt.foldmethod = 'expr'
vim.opt.foldexpr = 'nvim_treesitter#foldexpr()'
vim.opt.foldlevel = 99                          -- Start with all folds open

vim.cmd "set whichwrap+=<,>,[,],h,l"
vim.cmd [[set iskeyword+=-]]
vim.cmd [[set formatoptions-=cro]]              -- TODO: this doesn't seem to work

-- Highlight on yank
vim.api.nvim_create_autocmd('TextYankPost', {
  desc = 'Highlight when yanking (copying) text',
  group = vim.api.nvim_create_augroup('kickstart-highlight-yank', { clear = true }),
  callback = function()
    vim.highlight.on_yank()
  end,
})

-- Custom Welcome Message Logic
local function show_welcome()
    -- Only show if Neovim is opened without file arguments
    if vim.fn.argc() > 0 or vim.fn.line2byte('$') ~= -1 or #vim.api.nvim_list_wins() > 1 then
        return
    end

    local buf = vim.api.nvim_create_buf(false, true)
    local welcome_text = {
        "",
        "  ███╗   ██╗███████╗ ██████╗ ██╗   ██╗██╗███╗   ███╗ ",
        "  ████╗  ██║██╔════╝██╔═══██╗██║   ██║██║████╗ ████║ ",
        "  ██╔██╗ ██║█████╗  ██║   ██║██║   ██║██║██╔████╔██║ ",
        "  ██║╚██╗██║██╔══╝  ██║   ██║╚██╗ ██╔╝██║██║╚██╔╝██║ ",
        "  ██║ ╚████║███████╗╚██████╔╝ ╚████╔╝ ██║██║ ╚═╝ ██║ ",
        "  ╚═╝  ╚═══╝╚══════╝ ╚═════╝   ╚═══╝  ╚═╝╚═╝     ╚═╝ ",
        "",
        "             Welcome back, " .. os.getenv("USER") .. "!",
        "",
        "        [e] New File    [f] Find File    [q] Quit",
    }

    -- Set the lines in the buffer
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, welcome_text)

    -- Buffer options to make it feel like a dashboard
    vim.api.nvim_set_option_value("buftype", "nofile", { buf = buf })
    vim.api.nvim_set_option_value("bufhidden", "wipe", { buf = buf })
    vim.api.nvim_set_option_value("number", false, { scope = "local" })
    vim.api.nvim_set_option_value("relativenumber", false, { scope = "local" })

    -- Switch to the new buffer
    vim.api.nvim_set_current_buf(buf)
    
    -- Optional: simple keybindings for the dashboard
    local opts = { buffer = buf, silent = true }
    vim.keymap.set('n', 'e', ':enew<CR>', opts)
    vim.keymap.set('n', 'f', ':FzfLua files<CR>', opts) -- Requires FzfLua
    vim.keymap.set('n', 'q', ':qa<CR>', opts)
end

-- Run the function when Neovim starts
-- vim.api.nvim_create_autocmd("VimEnter", {
--     callback = show_welcome
-- })

require 'keymaps'
require 'plugins'
require 'colorscheme'
