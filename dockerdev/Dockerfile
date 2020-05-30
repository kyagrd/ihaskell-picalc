# Dockerfile for mybinder.org
#
# Test this Dockerfile:
#
#     docker build -t ihaskell-picalc .
#     docker run --rm -p 8888:8888 --name ihaskell_notebook --env JUPYTER_TOKEN=x ihaskell-picalc:latest
#

# FROM crosscompass/ihaskell-notebook:e763dc764d90
FROM crosscompass/ihaskell-notebook:fb96a81230df

USER root
RUN jupyter labextension install @jupyterlab/katex-extension
RUN mkdir /home/$NB_USER/picalc
COPY *.ipynb /home/$NB_USER/picalc/
RUN fix-permissions /home/$NB_USER/picalc
COPY global-project.stack.yaml $STACK_ROOT/global-project/stack.yaml
RUN stack install unbound-generics uglymemo lens tree-view data-partition \
 && stack build $STACK_ARGS ihaskell \
 && stack build $STACK_ARGS ghc-parser \
 && stack build $STACK_ARGS ipython-kernel \
 ## ihaskell-display
 && stack build $STACK_ARGS ihaskell-aeson \
 && stack build $STACK_ARGS ihaskell-blaze \
 && stack build $STACK_ARGS ihaskell-charts \
 && stack build $STACK_ARGS ihaskell-diagrams \
 && stack build $STACK_ARGS ihaskell-gnuplot \
 && stack build $STACK_ARGS ihaskell-graphviz \
 && stack build $STACK_ARGS ihaskell-hatex \
 && stack build $STACK_ARGS ihaskell-juicypixels \
# && stack build $STACK_ARGS ihaskell-magic \
# && stack build $STACK_ARGS ihaskell-plot \
# && stack build $STACK_ARGS ihaskell-rlangqq \
# && stack build $STACK_ARGS ihaskell-static-canvas \
# Skip install of ihaskell-widgets, they don't work.
# See https://github.com/gibiansky/IHaskell/issues/870
# && stack build $STACK_ARGS ihaskell-widgets \
 && stack build $STACK_ARGS hvega \
 && stack build $STACK_ARGS ihaskell-hvega \
 && fix-permissions $STACK_ROOT/global-project \
 && fix-permissions /opt/IHaskell \
 && fix-permissions /opt/hvega

USER $NB_UID

ENV JUPYTER_ENABLE_LAB=yes
