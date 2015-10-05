hi MyNumber ctermfg=2 guifg=SeaGreen
hi MyConstant ctermfg=2 guifg=SeaGreen gui=bold
hi GoTo cterm=bold gui=bold ctermfg=93 guifg=Purple
hi Mark term=bold cterm=bold ctermfg=7 ctermbg=93 gui=bold guibg=Purple guifg=Yellow
hi Operator ctermfg=6 guifg=Brown gui=Bold
hi MyKeyword ctermfg=3 guifg=Brown
hi MyTitle term=bold ctermfg=5 gui=bold guifg=Magenta

syn match MyTitle '^\s*Tags\(\s*\n\s*{\)\@='
syn match MyTitle '^\s*\(Init\|Sequence\)\s\+.\+\(\s*\n\s*{\)\@=' contains=MyConstant
syn match MyKeyword '\*\(Start\|Break\|Prio\|Delay\|Disable\|State\|Sequence\|Type\)\>'
syn match Macro '\*\(define\|include\)\>'
syn match MyKeyword 'with TimeBreak'
syn keyword MyKeyword MCS alias WaitFor InfoPv goto Nop Return Delay Pulse
syn keyword MyKeyword float int string
syn keyword MyKeyword if then else endif for to do done
syn region String   start=+'+  skip=+\\'+  end=+'+
syn match Operator '\([-+=!<>&|]\|:=\|AND\|OR\)'
syn match MyNumber '\<\d\+\>'
syn match MyNumber '\<0x[a-fA-F0-9]\+\>'
syn match MyNumber '\<\d\+\.\d\+\>'
syn match Mark '^[_A-Za-z][^:]*:[ \t\n]'
syn match GoTo '\(goto\s\)\@<=[_A-Za-z]\S*'
syn match Comment '\(^#\|[^$]#\).*' contains=ToDo
syn keyword Function PauseGroup StopGroup Val Log
syn match MyConstant '\$\S'
syn match ToDo 'TODO' contained
