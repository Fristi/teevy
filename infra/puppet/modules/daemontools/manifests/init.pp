class daemontools {

  include daemontools::package

  class {'daemontools::config':
    require => Class['daemontools::package'],
  }

}
