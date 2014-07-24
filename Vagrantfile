# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
    config.vm.box = "ubuntu12x64"
    config.vm.box_url = "http://puppet-vagrant-boxes.puppetlabs.com/ubuntu-server-12042-x64-vbox4210.box"
    
    config.vm.provider "virtualbox" do |v|
      v.memory = 1024
    end

    config.vm.network "private_network", ip: "10.1.2.3"
    config.vm.hostname = "teevy"
    
    config.vm.synced_folder "packaging", "/work/packaging", create: true
    config.vm.synced_folder "backend", "/work/backend", create: true
    config.vm.synced_folder "frontend", "/work/frontend", create: true
    config.vm.synced_folder "releases", "/work/releases", create: true
    
	config.vm.synced_folder "frontend/static", "/srv/www.teevy.co/static", create: true

    config.vm.provision :shell, :inline => "apt-get update"
    config.vm.provision :puppet do |puppet|
        puppet.manifests_path = "infra/puppet/manifests"
        puppet.manifest_file  = "init.pp"
        puppet.module_path = "infra/puppet/modules"
        puppet.hiera_config_path = "infra/puppet/manifests/hiera-dev.yaml"
        puppet.options = "--verbose --debug"
    end
    
    if Vagrant.has_plugin?("vagrant-cachier")
        config.cache.scope = :box
    end
end
