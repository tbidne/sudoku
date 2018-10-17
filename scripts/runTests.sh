#!/bin/bash

cd backend
stack build --ghc-options="-O2 -Wall -Werror -fwarn-tabs" --test :spec

cd ../ui
npm run build
npm test