## Generating protobuf

```
(cd proto; compile-proto-file --proto ratelimit.proto --out ../lib/Ratelimit/)

# compile-proto-file is weird and you can't get it to generate a module with
# a normal name, so we have to resort to sed shenanigans
mv lib/Ratelimit/Ratelimit.hs lib/Ratelimit/Proto.hs
sed -i 's/module Ratelimit/module Ratelimit.Proto/' lib/Ratelimit/Proto.hs
sed -i 's/Ratelimit\./Ratelimit\.Proto\./' lib/Ratelimit/Proto.hs
```
