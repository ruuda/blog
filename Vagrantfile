Vagrant.configure("2") do |config|
  config.vm.box = "ogarcia/archlinux-201504-x64"
  config.vm.provision :shell do |s|
    s.inline = "pacman -S --noconfirm ghc cabal-install"
  end
  config.vm.provision :shell do |s|
    s.inline = "cd /vagrant &&\
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
