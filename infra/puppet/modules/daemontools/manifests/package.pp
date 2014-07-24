class daemontools::package {

  package {'daemontools':
    ensure => present,
  }

  package {'daemontools-run':
    ensure  => present,
    require => Package['daemontools'],
    notify  => Exec['daemontools-start'],
  }

  exec {'daemontools-start':
    command     => "/usr/bin/sudo bash -cf '/usr/bin/svscanboot &'",
    refreshonly => true,
    require     => Package['daemontools'],
  }

}
