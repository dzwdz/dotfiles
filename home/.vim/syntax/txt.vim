if exists("b:current_syntax")
else
	let b:current_syntax = "txt"
endif


syn match Whitespace '^#.*'
syn match Todo '^!.*'
syn match Pmenu '^?.*'
