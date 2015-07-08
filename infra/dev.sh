#! /bin/bash

case "$1" in
  up)     cd env-dev
          docker-compose up -d
          sleep 3
          docker exec -it envdev_dev_1 /bin/bash
          ;;
  down)   cd env-dev
          docker-compose stop
          ;;
esac
