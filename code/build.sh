#! /bin/bash
set -e
cd "${0%/*}"
cd client && ./build.sh
cd ../api && ./build.sh