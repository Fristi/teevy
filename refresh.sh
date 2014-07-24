#!/bin/bash

part=$1

if [ -z "$part" ]; then
    echo "Please specify an action: $ sh refresh.sh updater-hard"
    exit -1
fi

vagrant ssh -c "sudo sh /work/backend/refresh-$part.sh"