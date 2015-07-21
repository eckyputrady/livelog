# LiveLog - [Live Demo](http://128.199.156.254/)

## Overview

LiveLog helps you improve your productivity. You log your activities and tag them appropriately for you to review overall at the end of the day. It is a much simpler version of [toggl](https://toggl.com/) in which you don't start & stop the timer, you will always be logging. Time. is. precious.

This is also a project for me to experiment with new technologies:

1. [CycleJS](http://cycle.js.org/), [virtual-dom](https://github.com/Matt-Esch/virtual-dom), and [MaterializeCSS](http://materializecss.com/) - for the client side
2. Haskell & [Scotty](https://hackage.haskell.org/package/scotty) - for the backend side
3. Docker for Infrastructure as Code

[!Login](screenshots/0.png)

[!Logs](screenshots/1.png)

## Setup

1. Install docker & docker-compose
2. For development:
    1. `./llg dev up`
    2. `./llg dev enter`
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