data Tree = Node Tree Tree | Leaf Taxon

data State = State Tree Size [(Tree, Float)]

type Taxon = Char
