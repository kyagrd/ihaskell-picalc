# How to run this repo

Should have access to a system where you can run Docker images.

FYI, on Debian or Ubuntu, you can use the following command to install and activate docker
```bash
sudo apt-get update -y
sudo apt-get install docker.io
sudo service docker start
sudo adduser $(whoami) docker   # may need re-login after this
```

Once you have Docker, you can simply `./run.sh` to run the Jupyter server equipped with IHaskell in a docker container.
On first run, it would take some time to pull the docker image from the Internet. You need about 10GB (not mor though) of disk space for this iamge. After the server is successfullly launched, you can connect to the IP address of the machine that is running the Docker image on port 8888. So, if you are running it on your local machine, it would be `localhost:8888`; and if you are running it on a yet another layer of virtualized environment (e.g. linux installed via WSL2 on Windows 10), then you should connect to that IP adresss's port 8888. The token you should input to Log-in is `x`. Then, the Jupyter will list some directories including `pwd`. Go inside `pwd` and click on `PiCalcOpenBisim.ipynb`.


# Below is a copy of the README.md from ihaskell-pwd temlplate
A pwd directory template for the [crosscompass/ihaskell-notebook](https://github.com/jamesdbrock/ihaskell-notebook) docker image
with some utility shell scripts.

Assuming there is a running Docker service set up for you, you can use
  * `run.sh` to run the ihaskell-notebook docker image from this directory.
  * `install.sh` inside the docker image's `pwd` directory. (use Jupyter's terminal for this) Here you can list the LTS packages you want to use in your notebook.

These shell scripts are poor man's Docker volume. If you are accustomed to Docker volumes, you may as well go ahead and make a volume for the snapshot directory of the container and copy the content from the Docker to initialize that volume.

Additionally, `wsl2browser.sh` is a script specifically for those who are running the ihaskell-notebook docker image on WSL2 and has google chrome browser installed in the usual directory. After the sucessfull `run.sh`, you can run `wsl2browser.sh` from the WSL2 linux shell.

As a demo, I specified the [unbound-gnereics](http://hackage.haskell.org/package/unbound-generics) in `install.sh` and provided an IHaskell notebook `LamdaClac.ipynb` adopted from one of the examples in the unbound-gnereics repo.

For more basic Haskell example notebooks, see https://github.com/jamesdbrock/learn-you-a-haskell-notebook
