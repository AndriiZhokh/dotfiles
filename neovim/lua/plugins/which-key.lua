return {
  'folke/which-key.nvim',
  event = 'VimEnter',
  config = function()
    require('which-key').setup {
      icons = {
        mappings = false,
        separator = '->',
      },
    }
    require('which-key').add {
      { '<leader>c', group = '[C]ode' },
      { '<leader>d', group = '[D]ocument' },
      { '<leader>r', group = '[R]ename' },
      { '<leader>s', group = '[S]earch' },
      { '<leader>w', group = '[W]orkspace' },
      { '<leader>t', group = '[T]oggle' },
      { '<leader>h', group = 'Git [H]unk', mode = { 'n', 'v' } },
      { '<leader>b', group = '[B]uffer' },
      { '<leader>m', group = '[M]ini' },
      { '<leader>f', group = '[F]ind' },
      { '<leader>n', group = '[N]otes' },

      { '<leader>bd', ':bd<CR>', desc = '[D]elete current buffer' }
    }
  end,
}
