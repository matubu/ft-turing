# Turing machine

## Encoding

blank = .
allowed alph: .01+yn
states names: a, b, c, d
pointer: @

[...transitions] | [...input]
transition = [WRITE] [TO_STATE] [CURRENT_STATE] [READ] :

fake blank for transition config = _
start state = a
end state = d

## Generate turing machine

```bash
deno run main.js > turing.json
```

## Unary addition

```bash
ft-turing turing.json "_ba1:_da_:_da+:1bb1:1db_:1db+:|1+1"
```

## 02n

```bash
ft-turing turing.json "_ba0:yda_:_ab0:ndb_:|00"
```