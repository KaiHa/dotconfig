hi Node term=bold cterm=bold ctermfg=5 gui=bold,underline guifg=orange3
hi Operator term=bold ctermfg=2 gui=bold guifg=SeaGreen

syn match Node '^\(Datapoint\|Formula\(Param\)\?\|FreeText\|OnTag\|Organisator\|Picture\)\s.*'
syn match IncSearch '\s*EOF FILE'
syn match IncSearch '\s*File \d\+ Ascii'
syn match Typedef '\$[A-Z]\+' contained
syn region String start='"' end='"' skip='\\"' contains=Typedef
syn region String start='\'' end='\'' skip='\\\'' contains=Typedef
syn match Tag '^\(OB\|GRP\)\(\s{\)\@='
syn match Function '^[A-Z]:'
syn keyword Conditional if else for do while 
syn keyword Type float int ptr string
syn match Operator '\(=\|\*\|\/\|<=\|>=\|&\|!=\|+\|-\|<\|>\||\|!\)'
syn match Comment '//.*' contains=ToDo
syn match ToDo 'TODO' contained
syn match Tag '<GROUP \(START\|END\)>'
"syn match Constant '\A\d\+\(\.\d\+\)\?\A\@='

let b:match_words = '<GROUP START>:<GROUP END>'

syn region vimFoldFile
      \ start="\s*File \d\+ Ascii"
      \ end="\s*EOF FILE"
      \ keepend
      \ transparent fold

