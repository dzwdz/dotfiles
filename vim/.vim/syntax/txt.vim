if exists("b:current_syntax")
	finish
endif

let b:current_syntax = "txt"

syn match Whitespace '^#.*'
syn match Todo '^!.*'
syn match WarningMsg '\*.\+\*'
