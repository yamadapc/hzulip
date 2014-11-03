# -*- mode: ruby -*-
# vi: set ft=ruby :

VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  config.vm.box = "phusion/ubuntu-14.04-amd64"

  cmd = "apt-get update && " \
        "apt-get install -q -y haskell-platform && " \
        "cabal update && " \
        "cabal install cabal cabal-install && " \
        "echo 'export PATH=$HOME/.cabal/bin:$PATH' >> $HOME/.bashrc"

  config.vm.provision :shell, :inline => cmd
end
