cp ../*.ipynb .
docker build --tag ihaskell:picalc .
docker tag ihaskell:picalc kyagrd/ihaskell:picalc
