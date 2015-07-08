# LiveLog

## Overview

LiveLog helps you log your activities throughout the day. It is a much simpler version of [toggl](https://toggl.com/).

And is actually a project for me to experiment with Haskell & Docker.

## Setup

1. Install docker & docker-compose
2. For development:
    1. `./llg dev up`
    2. Do `cabal repl` or `cabal build` as needed
    3. `./llg dev down` when done
3. For building & packaging (Continuous Integration):
	1. `./llg build up`
4. For running the build on local:
	1. `./llg build up`
	2. `./llg deploy up`
	3. go to `localhost` on your browser
	4. `./llg deploy down` when done