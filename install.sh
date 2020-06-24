## run this inside docker to install dependencies

# specify space sparated list of packages you need here

DEPS="unbound-generics tree-view data-partition uglymemo lens bimap ihaskell-graphviz"

(stack exec ghc-pkg -- list 'tree-view' | grep 'tree-view') \
	|| (cp stackDOTyaml /opt/stack/global-project/stack.yaml && stack install $DEPS)
