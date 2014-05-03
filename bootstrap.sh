#!/bin/bash

# provision script for ubuntu-14

# take any security upgrades
sudo apt-get -y dist-upgrade
# needed later for postgresql-simple
sudo apt-get -y install libpq-dev
sudo apt-get -y install postgresql-9.3
sudo apt-get -y install haskell-platform

# upgrade cabal
cabal update
cabal install Cabal-1.20.0.0 cabal-install-1.20.0.0
# cabal would install this, anyway
# cabal install postgresql-simple