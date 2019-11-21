# ratelimit

This document provides instructions on how to run the [Ratelimit
service](https://github.com/lyft/ratelimit) and how to submit requests
to it. Just like Fencer, Ratelimit is a [gRPC](https://grpc.io/)
service.

## Building ratelimit

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

## Run ratelimit (example config)

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

## Run ratelimit (custom config)

To run Ratelimit, configuration files that give rate limit rules are
needed. Ratelimit expects YAML configuration files. Files are searched
and loaded recursively from a base directory. The base directory is
given by the `RUNTIME_ROOT` environment variable, its subdirectory
given by the `RUNTIME_SUBDIRECTORY` environment variable and by its
subdirectory `config/`. For example, if the runtime root directory is
`./example/` and the runtime subdirectory is `ratelimit/`, Ratelimit
expects configuration files in the `./example/ratelimit/config/`
directory. The same applies to Fencer.

With the example values as above and the file and directory structure
as this:

```
.
├── current -> config1    # symlink to ./config1
└── config1
    └── ratelimit
        └── config
            ├── some_rule.yaml
            └── another_rule.yaml
```

one would start the Ratelimit server as follows:

```
LOG_LEVEL=debug REDIS_SOCKET_TYPE=tcp REDIS_URL=localhost:6379 RUNTIME_ROOT=current RUNTIME_SUBDIRECTORY=ratelimit ./result/bin/ratelimit
```

To test Ratelimit with a different configuration, create a directory
structure similar to the one above, change the symbolic link `current`
to point to the new runtime root directory and start the Ratelimit
server again, adjusting the environment variables accordingly.

## Running a gRPC client

To interact with a gRPC server such as Ratelimit and Fencer, a gRPC
client is needed. For example, the
[grpcurl](https://github.com/fullstorydev/grpcurl) tool can be
used. 

1. Change the current directory to one where gRPC protocol files can
   be found, e.g., in a Fencer repository directory:

    ```bash
    cd fencer
    ```

1. To submit a request to a running gRPC server (be it Ratelimit or
   Fencer) and consequently reduce the available amount of requests to
   be made, run:

    ```
    nix-shell -p grpcurl 
    ...
    > grpcurl -proto proto/rls.proto -plaintext -d '{"domain":"basic", "descriptors":[{"entries":[{"key":"key1"}]}]}' localhost:8081 envoy.service.ratelimit.v2.RateLimitService.ShouldRateLimit
    ```

   If everything went fine, you should get an OK response informing
   you of the limit remaining.


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
