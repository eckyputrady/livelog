# LiveLog

## Overview

Log your life.

Actually just a pet project for learning haskell + docker.

## Setup

1. Install docker & docker-compose
2. For development:
    1. `./llg dev up`
    2. Do `cabal repl` or `cabal build` as needed
    3. './/llg dev down' when done
3. For building & packaging (Continuous Integration):
	1. './llg build'
4. For running the build on local:
	1. './llg build'
	2. './llg deploy up'
	3. access `localhost`
	4. './llg deploy down' when done