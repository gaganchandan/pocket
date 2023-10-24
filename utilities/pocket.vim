syn keyword Keyword if else pass while function return print read not and or int char bool intlist charlist string boollist none true false

syn match Comment "^--.*"
syn match String "\".*\""
syn match Character "'.'"
syn match Number "\<[-]\=\d\+\>"

hi def link Keyword Statement
hi def link Comment Comment
hi def link String String 
hi def link Character String
hi def link Number Number
