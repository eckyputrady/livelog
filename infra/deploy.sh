#! /bin/bash

case "$1" in
  up)     cd env-deploy
          docker-compose up -d
          ;;
  down)   cd env-deploy
          docker-compose stop
          ;;
esac
