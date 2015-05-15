Vagrant.configure("2") do |config|
  config.vm.box = "ubuntu/vivid64"
  config.vm.provision :shell do |s|
    s.inline = "add-apt-repository --yes ppa:hvr/ghc &&\
                apt-get update &&\
                apt-get install --yes cabal-install-1.22 ghc-7.10.2 libz-dev"
  end
  config.vm.provision :shell do |s|
    s.inline = "PATH=\"$PATH:/opt/ghc/7.10.2/bin\" &&\
                PATH=\"$PATH:/opt/cabal/1.22/bin\" &&\
                echo 'PATH=\"$PATH:/opt/ghc/7.10.2/bin\"' >> /home/vagrant/.profile &&\
                echo 'PATH=\"$PATH:/opt/cabal/1.22/bin\"' >> /home/vagrant/.profile &&\
                cd /vagrant &&\
                cabal update &&\
                cabal sandbox init &&\
                cabal install -j --force-reinstalls"
    s.privileged = false
  end
  config.vm.provider "virtualbox" do |v|
    v.memory = 4096
    v.cpus = 6
  end
end
