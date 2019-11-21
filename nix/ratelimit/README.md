# ratelimit

To build lyft/ratelimit:

```
nix-build
```

This will create the three ratelimit binaries under the `./result/bin` directory, in addition
to a `ratelimit-server-example` script that will launch the ratelimit service
using the "examples/ratelimit" configuration in the ratelimit source repo.

### Testing the build

To run the service with the aforementioned example config:

```
USE_STATSD=false ./result/bin/ratelimit-server-example
```

Alternatively you can also use `nix-shell` to drop into the shell. This includes
other runtime dependencies like `grpcurl`.

## Run redis-server

``` sh
docker run -d -p 6379:6379 --name redis redis
```

## Run statsd

Using [this Docker
image](https://github.com/hopsoft/docker-graphite-statsd#docker-image-for-graphite--statsd):

``` sh
docker run -d\
 --name graphite\
 --restart=always\
 --env STATSD_INTERFACE=tcp\
 -p 80:80\
 -p 81:81\
 -p 2003-2004:2003-2004\
 -p 2023-2024:2023-2024\
 -p 8125:8125/tcp\
 -p 8126:8126\
 hopsoft/graphite-statsd
```

NOTE: We pass `STATSD_INTERFACE=tcp` because the docker image runs statsd with UDP
by default, however ratelimit tries to connect to statd via TCP.

## Run ratelimit

Now run ratelimit:

``` sh
./result/bin/ratelimit-server-example
```

It should run without any noticeable errors and connect successfully to both
redis and statsd services launched above.

To verify that ratelimit is successfully sending stats to the statd daemomn, 

1. Go to the dashboard http://localhost:81/dashboard 
1. Click on `stats.`
1. Expect to see the `stats.ratelimit.` key. 
1. Click that key to see the visualizations for the metrics being sent by ratelimit.

### Nix FAQ

## How to upgrade ratelimit

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

docker run -d\
 --name graphite\
 --restart=always\
 --env STATSD_INTERFACE=tcp\
 -p 80:80\
 -p 81:81\
 -p 2003-2004:2003-2004\
 -p 2023-2024:2023-2024\
 -p 8125:8125/tcp\
 -p 8126:8126\
 hopsoft/graphite-statsd
