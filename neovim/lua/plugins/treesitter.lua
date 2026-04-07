-- Highlight, edit, and navigate code
return {
  'nvim-treesitter/nvim-treesitter',
  build = ':TSUpdate',
  lazy = false,
  priority = 1000,
  config = function()
    require('nvim-treesitter').setup({
      prefer_git = false,
      compilers = { "zig", "gcc", "clang" },
    })

    local ts = require('nvim-treesitter')

    -- Install parsers if missing (runs async, no-op if already installed)
    if ts.install then
      ts.install { 'bash', 'c', 'diff', 'html', 'lua', 'luadoc', 'markdown', 'markdown_inline', 'vim', 'vimdoc' }
    end
  end,
}
