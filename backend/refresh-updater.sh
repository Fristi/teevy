#!/bin/bash

cd "$(dirname "$0")"

sh updater/build.sh
cp /work/backend/updater/dist/build/teevy-updater/teevy-updater /srv/www.teevy.co/bin