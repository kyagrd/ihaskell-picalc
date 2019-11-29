# Dockerfile for mybinder.org

FROM kyagrd/ihaskell:picalc

USER root
RUN rm -rf /home/$NB_USER/picalc
RUN mkdir /home/$NB_USER/picalc
COPY *.ipynb /home/$NB_USER/picalc/
RUN chown --recursive $NB_UID:users /home/$NB_USER/picalc

USER $NB_UID

ENV JUPYTER_ENABLE_LAB=yes
