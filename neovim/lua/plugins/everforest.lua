return {
  'sainnhe/everforest',
  lazy = false,
  -- name = 'everforest',
  priority = 1000,
  init = function()
    vim.g.everforest_background = 'medium'
  end,
  config = function()
    vim.opt.background = 'dark'

    vim.api.nvim_set_hl(0, 'Folded', {
      bg = '#282828',
      fg = vim.api.nvim_get_hl(0, { name = 'Comment' }).fg,
      bold = true,
    })
  end,
}
