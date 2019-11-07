LAB="--env JUPYTER_ENABLE_LAB=yes"
TAG=e763dc764d90
HOSTPORT=8888

if [ -z "$(ls -A ".snapshots" 2>/dev/null)" ]; then
	docker run --rm -p $HOSTPORT:8888 $LAB \
		-v "$PWD":/home/jovyan/pwd \
		--env JUPYTER_TOKEN=x --name ihaskell_notebook \
	       	crosscompass/ihaskell-notebook:$TAG
else
	docker run --rm -p $HOSTPORT:8888 $LAB \
		-v "$PWD":/home/jovyan/pwd \
	       	-v "$PWD/.snapshots":/opt/stack/snapshots \
	       	-v "$PWD/.stack-work":/opt/stack/global-project/.stack-work \
		--env JUPYTER_TOKEN=x --name ihaskell_notebook \
	       	crosscompass/ihaskell-notebook:$TAG
fi
