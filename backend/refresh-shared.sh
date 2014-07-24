#!/bin/bash

cd "$(dirname "$0")"

sh database/build.sh
sh tmdb/build.sh
sh trakt/build.sh
sh shared/build.sh