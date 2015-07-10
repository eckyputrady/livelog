FROM haskell:7.8

RUN apt-get update && apt-get install -y libpcre3 libpcre3-dev libmysqlclient-dev && \
    cabal update && cabal install hlint

VOLUME /livelog
WORKDIR /livelog
EXPOSE 3000
