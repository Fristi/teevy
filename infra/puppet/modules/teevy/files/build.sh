#!/bin/bash

version=$1

if [ -z "$version" ]; then
    echo "Please specify a version: $ sh build.sh 1.0.0"
    exit -1
fi

rm -rf "versions/teevy_$version"

mkdir -p "versions/teevy_$version/DEBIAN"
mkdir -p "versions/teevy_$version/etc/teevy/releases/$version"
mkdir -p "versions/teevy_$version/srv/www.teevy.co/bin"
mkdir -p "versions/teevy_$version/srv/www.teevy.co/static"

sh ../backend/database/build.sh
sh ../backend/webservice/build.sh
sh ../backend/frontend/build.sh

cp -R "../releases/$version" "versions/teevy_$version/etc/teevy/releases"
cp ../backend/webservice/dist/build/teevy-webservice/teevy-webservice "versions/teevy_$version/srv/www.teevy.co/bin"
cp -R ../frontend/static "versions/teevy_$version/srv/www.teevy.co"

cp control.descriptor "versions/teevy_$version/DEBIAN/control"

cd versions

dpkg-deb --build "teevy_$version"