return {
  'nvimdev/dashboard-nvim',
  event = 'VimEnter',
  config = function()
    require('dashboard').setup {
      theme = 'doom',
      config = {
        header = {},  
        vertical_center = true,
      },
    }
  end,
  dependencies = { { 'nvim-tree/nvim-web-devicons' } }
}
