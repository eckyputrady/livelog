# LiveLog

## Overview

LiveLog helps you log your activities throughout the day. It is a much simpler version of [toggl](https://toggl.com/).

And is actually a project for me to experiment with Haskell & Docker.

## Setup

1. Install docker & docker-compose
2. For development:
    1. `./llg dev up`
    2. `./llg dev enter`
    2. see `code/client/README.md` && `code/api/README.md` to see how to develop client & API
    3. `./llg dev down` when done
3. For building & packaging (Continuous Integration):
	1. `./llg build up`
4. For running the build on local:
	1. `./llg build up`
	2. `./llg deploy up`
	3. go to `localhost` from your browser
	4. `./llg deploy down` when done

## Deployment

0. You should set the following credentials to yours:
   1. `code/client/client-key.aes`
   2. `infra/env-deploy/env` (the username & password to DB)
1. Install docker-machine
2. after creating & setting docker to point to appropriate machine, do `./llg deploy up`