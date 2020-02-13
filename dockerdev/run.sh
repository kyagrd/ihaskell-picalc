LAB="--env JUPYTER_ENABLE_LAB=yes"
TAG=picalc
HOSTPORT=8888

docker run --rm -p $HOSTPORT:8888 $LAB \
		--env JUPYTER_TOKEN=x --name ihaskell_notebook \
	       	ihaskell:$TAG
