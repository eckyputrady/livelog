#! /bin/bash
cd "$(dirname "$0")"

cat /etc/hosts
ghc --version
cabal --version

# cabal sandbox delete
cabal sandbox init
cabal update
cabal configure
cabal install --only-dependencies --enable-tests
cabal test --show-details=streaming
cabal build

echo "Packaging application ..."
mkdir -p dist/livelog
cp dist/build/livelog-exe/livelog-exe dist/livelog/livelog-exe
cp client_session_key.aes             dist/livelog/client_session_key.aes
echo "Packaging application DONE"
