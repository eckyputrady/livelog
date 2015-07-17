#! /bin/bash
cd "${0%/*}"
./client/build.sh
./api/build.sh
