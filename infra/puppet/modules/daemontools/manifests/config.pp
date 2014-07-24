class daemontools::config {

  file {'/etc/service':
    ensure  => directory,
    mode    => 0755,
    owner   => root,
    group   => root,
  }

}
