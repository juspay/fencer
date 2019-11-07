# ratelimit

To build lyft/ratelimit:

```
nix-build
```

This will create the three ratelimit binaries under the `./result/bin` directory, in addition
to a `ratelimit-server-example` script that will launch the ratelimit service
using the "examples/ratelimit" configuration in the ratelimit source repo.

To run the service with the aforementioned example config:

```
./result/bin/ratelimit-server-example
```

Alternatively you can also use `nix-shell` to drop into the shell. This includes
other runtime dependencies like `grpcurl`.

## How to update ratelimit

1. Determine the Git revision of upstream repo you'd like to update to

2. Generate the new `deps.nix` (from upstream's `glide.lock`):

To do this, you must clone the ratelimit repo, at the specific revision, under
 $GOPATH:
 
```

export GOPATH=~/wherever
mkdir -p $GOPATH/src/github.com/lyft
cd $GOPATH/src/github.com/lyft
git clone https://github.com/lyft/ratelimit.git
cd ratelimit
git chekckout $rev
```
 
and then run:

```
nix-shell -p dep dep2nix --run 'dep init && dep2nix'
```

3. Copy over the resulting `deps.nix` to this directory.

4. Set `rev` to the Git revision in default.nix.

5. Run `nix-build`; you should expect to see a sha256 error. Update default.nix
   with the new sha256.
   
6. Run `nix-build` again to confirm the new build.
