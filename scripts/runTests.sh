#!/bin/bash

cd $TRAVIS_BUILD_DIR/backend
stack build --ghc-options="-O2 -Wall -Werror -fwarn-tabs" --test :spec

cd $TRAVIS_BUILD_DIR/ui
npm run build
npm test