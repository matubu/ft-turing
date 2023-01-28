# Encoding

blank = .
allowed alph: 01+.
states names: a, b, c, d
pointer: @

[...transitions] | [...input]
transition = [WRITE] [TO_STATE] [CURRENT_STATE] [READ] :

fake blank for transition config = _
start state = a
end state = d

# Unary addition

_ba1:_da_:_da+:1bb1:1db_:1db+:|1+1

```bash
deno run main.js > turing.json && ft-turing turing.json "_ba1:_da_:_da+:1bb1:1db_:1db+:|1+1"
```