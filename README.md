# ihaskell-pwd
A pwd directory template for the [crosscompass/ihaskell-notebook](https://github.com/jamesdbrock/ihaskell-notebook) docker image
with some utility shell scripts.

Assuming there is a running Docker service set up for you, you can use
  * `run.sh` to run the ihaskell-notebook docker image from this directory.
  * `install.sh` inside the docker image's `pwd` directory. (use Jupyter's terminal for this) Here you can list the LTS packages you want to use in your notebook.

These shell scripts are poor man's Docker volume. If you are accustomed to Docker volumes, you may as well go ahead and make a volume for the snapshot directory of the container and copy the content from the Docker to initialize that volume.

Additionally, `wsl2browser.sh` is a script specifically for those who are running the ihaskell-notebook docker image on WSL2 and has google chrome browser installed in the usual directory. After the sucessfull `run.sh`, you can run `wsl2browser.sh` from the WSL2 linux shell.

As a demo, I specified the [unbound-gnereics](http://hackage.haskell.org/package/unbound-generics) in `install.sh` and provided an IHaskell notebook `LamdaClac.ipynb` adopted from one of the examples in the unbound-gnereics repo.

For more basic Haskell example notebooks, see https://github.com/jamesdbrock/learn-you-a-haskell-notebook
