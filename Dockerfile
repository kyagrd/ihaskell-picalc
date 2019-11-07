# Dockerfile for mybinder.org
#
# Test this Dockerfile:
#
#     docker build -t ihaskell-picalc .
#     docker run --rm -p 8888:8888 --name learn-you-a-haskell --env JUPYTER_TOKEN=x ihaskell-picalc:latest
#

FROM crosscompass/ihaskell-notebook:e763dc764d90

USER root

RUN mkdir /home/$NB_USER/picalc
COPY *.ipynb /home/$NB_USER/picalc/
RUN chown --recursive $NB_UID:users /home/$NB_USER/picalc

USER $NB_UID
RUN stack install unbound-generics tree-view data-partition uglymemo lens

ENV JUPYTER_ENABLE_LAB=yes
