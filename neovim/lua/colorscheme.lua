local COLORSCHEMES = {
  everforest = 'everforest',
  yugen = 'yugen',
  nord = 'nord',
  terafox = 'terafox',
  kanagawa_paper = 'kanagawa-paper',
  compline = 'compline'
}
local colorscheme = 'compline'

local status_ok, _ = pcall(vim.cmd, 'colorscheme ' .. COLORSCHEMES.compline)

if not status_ok then
    vim.notify('colorscheme ' .. colorscheme .. ' not found')
    return
end
