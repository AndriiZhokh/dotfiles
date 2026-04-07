-- Highlight, edit, and navigate code
return {
  'nvim-treesitter/nvim-treesitter',
  build = ':TSUpdate',
  lazy = false,
  priority = 1000,
  config = function()
    require('nvim-treesitter').setup()

    local ts = require('nvim-treesitter')

    -- Install parsers if missing (runs async, no-op if already installed)
    if ts.install then
      ts.install { 'bash', 'c', 'diff', 'html', 'lua', 'luadoc', 'markdown', 'markdown_inline', 'vim', 'vimdoc' }
    end

    -- Auto-install parser when opening a file with a missing parser
    -- vim.api.nvim_create_autocmd('FileType', {
    --   callback = function(ev)
    --     local lang = vim.treesitter.language.get_lang(ev.match) or ev.match
    --     if not pcall(vim.treesitter.language.inspect, lang) then
    --       if ts.install then
    --         local install = ts.install({ lang })
    --         if install and install.wait then
    --           install:wait(60000)
    --           -- Re-trigger FileType so ftplugin runs with the parser available
    --           vim.cmd('doautocmd FileType ' .. ev.match)
    --         end
    --       end
    --     end
    --   end,
    -- })
  end,
}
