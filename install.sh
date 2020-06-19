## run this inside docker to install dependencies

# specify space sparated list of packages you need here

DEPS="unbound-generics tree-view data-partition uglymemo lens ihaskell-graphviz"
cp stackDOTyaml /opt/stack/global-project/stack.yaml
stack install $DEPS
