#!/bin/bash

version=$1

if [ -z "$version" ]; then
    echo "Please specify a version: $ sh build.sh 1.0.0"
    exit -1
fi

#!/bin/bash
vagrant ssh -c "sudo sh /work/packaging/build.sh $version"

## scp "./packaging/versions/teevy_$version.deb" root@teevy.co:~/