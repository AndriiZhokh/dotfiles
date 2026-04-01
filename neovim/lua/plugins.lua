local lazypath = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'

if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    'git',
    'clone',
    '--filter=blob:none',
    'https://github.com/folke/lazy.nvim.git',
    '--branch=stable', -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

local plugins = {
  require 'plugins.treesitter',

  -- color schemes
  require 'plugins.everforest',
  require 'plugins.lemons',
  require 'plugins.nightfox',
  require 'plugins.catppuccin',
  require 'plugins.yugen',
  require 'plugins.nord',
  require 'plugins.vesper',
  require 'plugins.origami',
  require 'plugins.kanagawa-paper',

  require 'plugins.markview', -- brocken after updated to 0.12.0 of neovim
  require 'plugins.autopairs',
  require 'plugins.mini',
  require 'plugins.which-key',
  require 'plugins.telescope',
  require 'plugins.fzf-lua',

  -- TODO: to check new plugins
  -- https://github.com/krshrimali/context-pilot.nvim - git helper plugin
  -- https://github.com/folke/flash.nvim - for better navigation in file
}

require('lazy').setup(plugins)
