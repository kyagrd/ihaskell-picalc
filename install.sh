## run this inside docker to install dependencies

# specify space sparated list of packages you need here

DEPS="unbound-generics tree-view"

if [ -d "/opt/stack/snapshots" ]; then
	if [ -d ".snapshots" -a -d ".stack-work" ]; then
		stack install $DEPS
	else
		./clean.sh \
			&& echo "copying snapshots and global-projct/.stack-work to host" \
			&& mv /opt/stack/snapshots "$PWD/.snapshots" \
			&& ln -s "$PWD/.snapshots" /opt/stack/snapshots \
			&& mv /opt/stack/global-project/.stack-work "$PWD/.stack-work" \
			&& ln -s "$PWD/.stack-work" /opt/stack/global-project/ \
			&& stack install $DEPS
	fi
else
	echo "Directory /opt/stack/snapshots does not exist."
fi
