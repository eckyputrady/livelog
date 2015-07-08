#! /bin/bash

cd env-build
docker-compose up
cp -rf ../../livelog/dist/livelog ../haskell-deploy

cd ..
docker build -t ecky/livelog haskell-deploy
