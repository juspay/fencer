## Generating protobuf

```
# compile-proto-file is weird and you can't get it to generate a module with
# a normal name, so we have to resort to sed shenanigans
(cd proto; compile-proto-file --proto rls.proto --out ../lib/Ratelimit/) \
  && mv lib/Ratelimit/Rls.hs lib/Ratelimit/Proto.hs \
  && sed -i 's/module Rls/module Ratelimit.Proto/' lib/Ratelimit/Proto.hs \
  && sed -i 's/Rls\./Ratelimit\.Proto\./g' lib/Ratelimit/Proto.hs
```
