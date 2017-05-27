# Fasta parser

This is a simple FASTA parser that takes biological sequence in the format

```
>id1 description
GATACAGATACAGATACAGATACAGATACAGATACAGATACAGATACA
GATACAGATACAGATACAGATACAGATACAGATACAGATACAGATACA
GATACAGATACAGATACAGATACAGATACAGATACAGATACAGATACA
>id2 description la di da
GATACAGATACAGATACAGATACAGATACAGATACAGATACAGATACA
GATACAGATACAGATACAGATACAGATACAGATACAGATACAGATACA
GATACAGATACAGATACAGATACAGATACAGATACAGATACAGATACA
```

The grammar is

```
fasta    := entry | fasta entry
entry    := header sequence
header   := '>' line
sequence := line | sequence line
string   := c | string c
line     := c '\n'
c        := [^>\n]
```
