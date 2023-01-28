# Encoding

allowed alph: 1+.
states names: a, b, c
pointer: @

[123123][aa] | [i...]
a=alphabet
t=transition (TO_STATE, WRITE)
i=input

config_[aa]([to][wr])*4_[state]o

goto_to_ptr_[state][write]
search_conf_[state][read]