LAB="--env JUPYTER_ENABLE_LAB=yes"
TAG=picalc
HOSTPORT=8888

if [ -z "$(ls -A ".snapshots" 2>/dev/null)" ]; then
	docker run --rm -p $HOSTPORT:8888 $LAB \
		-v "$PWD":/home/jovyan/picalc \
		--env JUPYTER_TOKEN=x --name ihaskell_notebook \
	       	kyagrd/ihaskell:$TAG
else
	docker run --rm -p $HOSTPORT:8888 $LAB \
		-v "$PWD":/home/jovyan/picalc \
	       	-v "$PWD/.snapshots":/opt/stack/snapshots \
	       	-v "$PWD/.stack-work":/opt/stack/global-project/.stack-work \
		--env JUPYTER_TOKEN=x --name ihaskell_notebook \
	       	kyagrd/ihaskell:$TAG
fi
