#!/bin/sh
export PGPASSWORD='#@aDd##ad@323!@aa#%'

cd "$(dirname "$0")"

psql -U teevy --host=10.1.2.3 --dbname=teevy -f 1.0.0/teevy.sql
psql -U teevy --host=10.1.2.3 --dbname=teevy -f 1.0.1/indexes.sql
psql -U teevy --host=10.1.2.3 --dbname=teevy -f 1.0.2/indexes.sql
psql -U teevy --host=10.1.2.3 --dbname=teevy -f 1.0.3/drop_user_sessions.sql
psql -U teevy --host=10.1.2.3 --dbname=teevy -f 1.0.3/rename_subscription.sql
psql -U teevy --host=10.1.2.3 --dbname=teevy -f 1.0.3/facebook_users.sql
psql -U teevy --host=10.1.2.3 --dbname=teevy -f 1.0.4/to_users.sql