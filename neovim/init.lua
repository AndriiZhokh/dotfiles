require 'keymaps'
require 'plugins'
require 'colorscheme'

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

local option = vim.opt

-- :help options
local options = {
  backup = false,                          -- creates a backup file
  clipboard = "unnamedplus",               -- allows neovim to access the system clipboard
  cmdheight = 1,                           -- more space in the neovim command line for displaying messages
  completeopt = { "menuone", "noselect" }, -- mostly just for cmp
  conceallevel = 0,                        -- so that `` is visible in markdown files
  colorcolumn = "140",
  fileencoding = "utf-8",                  -- the encoding written to a file
  hlsearch = true,                         -- highlight all matches on previous search pattern
  ignorecase = true,                       -- ignore case in search patterns
  mouse = "a",                             -- allow the mouse to be used in neovim
  pumheight = 10,                          -- pop up menu height
  showmatch = true,
  showmode = false,                        -- we don't need to see things like -- INSERT -- anymore
  showtabline = 2,                         -- always show tabs
  smartcase = true,                        -- smart case
  smartindent = true,                      -- make indenting smarter again
  splitbelow = true,                       -- force all horizontal splits to go below current window
  splitright = true,                       -- force all vertical splits to go to the right of current window
  swapfile = false,                        -- creates a swapfile
  termguicolors = true,                    -- set term gui colors (most terminals support this)
  timeoutlen = 100,                        -- time to wait for a mapped sequence to complete (in milliseconds)
  undofile = true,                         -- enable persistent undo
  updatetime = 300,                        -- faster completion (4000ms default)
  writebackup = false,                     -- if a file is being edited by another program (or was written to file while editing with another program), it is not allowed to be edited
  expandtab = true,                        -- convert tabs to spaces
  shiftwidth = 2,                          -- the number of spaces inserted for each indentation
  tabstop = 2,                             -- insert 2 spaces for a tab
  cursorline = true,                       -- highlight the current line
  number = true,                           -- set numbered lines
  relativenumber = true,                   -- set relative numbered lines
  numberwidth = 2,                         -- set number column width to 2 {default 4}
  signcolumn = "yes",                      -- always show the sign column, otherwise it would shift the text each time
  wrap = false,                            -- display lines as one long line
  scrolloff = 8,                           -- is one of my fav
  sidescrolloff = 8,
  guifont = "monospace:h17",               -- the font used in graphical neovim applications
  foldenable = true,                       -- enables folding
  foldmethod = FoldMethods.manual,         -- set default folding method
  -- nofoldenable = false                  -- disable folding
  winborder = 'rounded'
}

option.shortmess:append "c"

for k, v in pairs(options) do
  option[k] = v
end

vim.opt.foldmethod = 'expr'
vim.opt.foldexpr = 'nvim_treesitter#foldexpr()'
vim.opt.foldlevel = 99 -- Start with all folds open

vim.cmd "set whichwrap+=<,>,[,],h,l"
vim.cmd [[set iskeyword+=-]]
vim.cmd [[set formatoptions-=cro]] -- TODO: this doesn't seem to work

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
    if vim.fn.argc() > 0 or vim.fn.line2byte('$') ~= -1 or table.getn(vim.api.nvim_list_wins()) > 1 then
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
vim.api.nvim_create_autocmd("VimEnter", {
    callback = show_welcome
})
