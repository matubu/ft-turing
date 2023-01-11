run: all
	@echo "\n---- FT-TURING ----\n"
	@ft-turing machine/unary_sub.json "111-11="

all:
	stack build --copy-bins