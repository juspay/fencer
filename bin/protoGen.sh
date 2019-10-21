#!/usr/bin/env bash

set -e

(cd proto; compile-proto-file --proto rls.proto --out ../lib/Fencer/)
mv lib/Fencer/Rls.hs lib/Fencer/Proto.hs
sed -i 's/module Rls/module Fencer.Proto/' lib/Fencer/Proto.hs
sed -i 's/Rls\./Fencer\.Proto\./g' lib/Fencer/Proto.hs
sed -i '1s/^/{- HLINT ignore -}\n\n/' lib/Fencer/Proto.hs
DIFF=$(git diff lib/Fencer/Proto.hs | wc -c)
if [ "$DIFF" != "0" ]; then exit 1; fi
