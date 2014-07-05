"haskellmode
let g:haddock_browser="/usr/bin/elinks"
compiler ghc

"neco
setlocal omnifunc=necoghc#omnifunc

"hdevtools
nnoremap <buffer> <F2> :HdevtoolsType<CR>
nnoremap <buffer> <silent> <F3> :HdevtoolsClear<CR>
