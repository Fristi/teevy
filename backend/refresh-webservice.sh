#!/bin/bash

cd "$(dirname "$0")"

sh haskell-jwt/build.sh
sh webservice/build.sh

## WEBSERVICE
sudo svc -d /etc/service/teevy-webservice
cp /work/backend/webservice/dist/build/teevy-webservice/teevy-webservice /srv/www.teevy.co/bin
sudo svc -u /etc/service/teevy-webservice