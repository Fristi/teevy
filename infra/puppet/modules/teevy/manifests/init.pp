
class teevy::base {

  $profile = $teevy::profile

  if($profile == "dev") {
    package { 'libpq-dev': }
    package { 'haskell-platform': }
  
    $user = "vagrant"
    $home = "/home/$user"

    # cabal update
    exec { 'cabal update' :
      user        => $user,
      environment => "HOME=$home",
      path        => ['/bin', '/usr/bin', '/usr/local/bin'],
      require     => Package["haskell-platform"]
    }

    # cabal upgrade
    exec { 'cabal install cabal-install' :
      user        => $user,
      environment => "HOME=$home",
      path        => ['/bin', '/usr/bin', '/usr/local/bin'],
      require     => Exec['cabal update']
    }
  }

  if($profile == "prod") {
    package { 'ghc': }
  }

  file { ["/srv/www.teevy.co","/srv/www.teevy.co/static","/srv/www.teevy.co/bin"]: 
    ensure => "directory"
  }
}

##############
# WEBSERVICE #
##############

class teevy::webservice {
    $db_endpoint = $teevy::db_endpoint
    $db_teevy_pwd = $teevy::db_teevy_pwd
  
    file { "/srv/www.teevy.co/bin/teevy.conf":
      content => template("teevy/teevy.conf.erb"),
      require => File["/srv/www.teevy.co/bin"] 
    }   

    daemontools::service {'teevy-webservice':
      ensure  => running,
      command => "/srv/www.teevy.co/bin/teevy-webservice /srv/www.teevy.co/bin/teevy.conf",
      require => [File["/srv/www.teevy.co/bin/teevy.conf"]]
    }
}


#############
### WEB #####
#############

class teevy::web {

    file { "/srv/www.teevy.co/server.crt":
        source => "puppet:///modules/teevy/server.crt", 
        require => File["/srv/www.teevy.co"]
    }

    file { "/srv/www.teevy.co/server.pem":
        source => "puppet:///modules/teevy/server.pem",
        require => File["/srv/www.teevy.co"]
    }

    class { 'nginx': }

    nginx::resource::vhost { 'teevy.co':
      rewrite_to_https => true,
      rewrite_www_to_non_www => true,
      www_root    => '/srv/www.teevy.co/static',
      listen_port => 80,
      ssl_port    => 443,
      ipv6_enable => false,
      ssl         => true,
      ssl_cert    => '/srv/www.teevy.co/server.crt',
      ssl_key     => '/srv/www.teevy.co/server.pem',
      require     => [File["/srv/www.teevy.co/server.pem"], File["/srv/www.teevy.co/server.crt"]]
    }

    nginx::resource::location { "/api":
      ensure      => present,
      location    => "/api",
      rewrite_rules => ['^/api/(.*)$ /$1 break'],
      ssl         => true,
      ssl_only    => true,
      vhost       => 'teevy.co',
      proxy       => 'http://app',
      location_custom_cfg_append => ['proxy_set_header X-Real-IP $remote_addr;']
    }

    nginx::resource::upstream { 'app':
      members => [
        'localhost:3000'
      ],
    }
}


#############
### FW ######
#############

class teevy::firewall {
    $profile = $teevy::profile

    if($profile == 'prod') {

        include ufw

        ufw::allow { "allow-ssh-from-all": port => 22, ip => 'any' }
        ufw::allow { "https": port => 80, ip => 'any' }
        ufw::allow { "https": port => 443, ip => 'any' }

        ufw::limit { 22: }
    }
}

#############
### DB ######
#############

class teevy::db {

    $db_listen = $teevy::db_listen
    $db_acl = $teevy::db_acl
    $db_root_pwd = $teevy::db_root_pwd
    $db_teevy_pwd = $teevy::db_teevy_pwd

    class { 'postgresql::server':
        listen_addresses           => $db_listen,
        postgres_password          => $db_root_pwd,
        manage_pg_hba_conf         => true,
        ipv4acls                   => [$db_acl],
        manage_firewall            => true
    }

    postgresql::server::db { 'teevy': 
        user     => 'teevy',
        password => postgresql_password('teevy', $db_teevy_pwd)
    }  
}

#############
### LOG #####
#############

class teevy::logging {
    $token = $teevy::loggly_token

    class { 'loggly::rsyslog':
        customer_token => $token, 
    }
}

class teevy(
    $db_listen = "*",
    $db_endpoint = "10.1.2.3",
    $db_acl = "host teevy teevy 0.0.0.0/0 md5",
    $db_root_pwd = "teevy",
    $db_teevy_pwd = "teevy",
    $loggly_token = "",
    $profile = "dev"
) {
    class {'teevy::base': } ->
    class {'teevy::db': } ->
    class {'teevy::web': } ->
    class {'teevy::webservice': } ->
    class {'teevy::firewall': } ->
    class {'teevy::logging': }
}
