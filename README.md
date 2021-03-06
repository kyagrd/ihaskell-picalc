# How to run this repo

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/kyagrd/ihaskell-picalc/master?urlpath=lab)
[![NBviewer](./nbviewer_badge.svg)](https://nbviewer.jupyter.org/github/kyagrd/ihaskell-picalc/tree/master/)

The quickest way to try our system is clicking the `launch|binder` button above,
which runs the Docker image of our repository on the cloud.

For better performance, you can also run the Docker image on your machine locally
if you can run Docker in your machine.

FYI, on Debian or Ubuntu, you can use the following command to install and activate docker
```bash
sudo apt-get update -y
sudo apt-get install docker-ce
sudo service docker start
sudo adduser $(whoami) docker   # may need re-login after this
```

Once you have Docker, you can simply `./run.sh` to run the Jupyter server equipped with IHaskell in a docker container.
On first run, it would take some time to pull the docker image from the Internet. You need about 10GB (not mor though) of disk space for this iamge. After the server is successfullly launched, you can connect to the IP address of the machine that is running the Docker image on port 8888. So, if you are running it on your local machine, it would be `localhost:8888`; and if you are running it on a yet another layer of virtualized environment (e.g. linux installed via WSL2 on Windows 10), then you should connect to that IP adresss's port 8888. The token you should input to Log-in is `x`. Then, the Jupyter will list some directories including `picalc`. Go inside `picalc` and click on `PiCalcOpenBisim.ipynb`.
