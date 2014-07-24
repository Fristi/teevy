include harden

class { 'teevy':
    db_listen => hiera('db.listen'),
    db_acl => hiera('db.acl'),
    db_root_pwd => hiera('db.root.pwd'),
    db_teevy_pwd => hiera('db.teevy.pwd'),
    db_endpoint => hiera('db.endpoint'),
    loggly_token => hiera('loggly.token'),
    profile => hiera("profile")
}