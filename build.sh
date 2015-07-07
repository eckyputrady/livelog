#! /bin/bash
cat /etc/hosts
env
ghc --version
cabal --version

cabal sandbox init
cabal update
cabal configure
cabal install --only-dependencies --enable-tests
cabal test
cabal build

echo "Packaging application ..."
mkdir -p dist/livelog
cp dist/build/livelog-exe/livelog-exe dist/livelog/livelog-exe
cp config.json dist/livelog/config.json
cp client_session_key.aes dist/livelog/client_session_key.aes
echo "Packaging application DONE"