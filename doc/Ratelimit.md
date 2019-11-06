# Using lyft/ratelimit

This document provides instructions on how to run the [Ratelimit
service](https://github.com/lyft/ratelimit) and how to submit requests
to it. Just like Fencer, Ratelimit is a [gRPC](https://grpc.io/)
service.

## Installing Go

Ratelimit is written in the Go programming language. Therefore, a Go
compiler is needed to compile Ratelimit:

1. Install the [Go programming language](https://golang.org/). How to
   install Go is described in [Go's official
   documentation](https://golang.org/doc/install). An easy way to
   install it on Debian-based GNU/Linux systems is:

    ```bash
    sudo apt-get install golang
    ```

2. Make sure to have the `GOPATH` environment variable configured. It
   should be a path to a workspace directory for Go projects. For
   example, you could create a directory called `go` in your home
   directory and use it for your Go projects:

    ```bash
    mkdir -p ~/go
    export GOPATH=~/go
    ```

   Consider placing the export command to your shell configuration
   file, e.g., to `~/.bashrc`, to permanently have the `GOPATH`
   variable available in the environment.

## Installing Redis

Ratelimit depends on [Redis](https://redis.io/). [The official Redis
documentation](https://redis.io/download) has instructions for
downloading and installing the Redis server. On Debian-based GNU/Linux
systems run the following to install Redis and automatically start a
server on `localhost:6379`:

    ```bash
    sudo apt-get install redis-server
	```

Make sure to have the Redis server running on `localhost` on port
6379.

## Installing Ratelimit

After Go is installed, install the `lyft/ratelimit` server:

1. Clone its Git repository into the Go workspace directory in a
   subdirectory for the Ratelimit project:

    ```bash
	cd $GOPATH
	mkdir -p src/github.com/lyft
	cd src/github.com/lyft
    git clone https://github.com/lyft/ratelimit.git
    ```

2. Set it up for the first time. This needs to be done only once:

    ```bash
	cd ratelimit
	make bootstrap
	```

3. Compile Ratelimit (The `GOOS` variable is needed on OSX if running
   a Linux container):

    ```bash
	GOOS=linux make compile
	```

4. To compile and run tests:

    ```bash
    make tests
    ```

5. Go binaries are self-contained and no Go runtime environment is
   needed to run compiled Go programs. To make them system-wide
   available, copy them to a location that is on your `PATH`, e.g.:

    ```bash
	sudo cp bin/* /usr/local/bin/
    ```

## Running the Ratelimit Server

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

one would start the Ratelimit server as follows (assuming the
`ratelimit` executable is on your path):

```bash
USE_STATSD=false LOG_LEVEL=debug REDIS_SOCKET_TYPE=tcp REDIS_URL=localhost:6379 RUNTIME_ROOT=current RUNTIME_SUBDIRECTORY=ratelimit ratelimit
```

To test Ratelimit with a different configuration, create a directory
structure similar to the one above, change the symbolic link `current`
to point to the new runtime root directory and start the Ratelimit
server again, adjusting the environment variables accordingly.

## Installing and Running a gRPC Client

To interact with a gRPC server such as Ratelimit and Fencer, a gRPC
client is needed. For example, the
[grpcurl](https://github.com/fullstorydev/grpcurl) tool can be
used. It is implemented in Go so to use a pre-compiled binary for
Linux-based operating systems:

1. Go to a temporary directory, e.g.:

    ```bash
    cd /tmp
    ```

2. Download a binary release of `grpcurl`:

    ```bash
    wget https://github.com/fullstorydev/grpcurl/releases/download/v1.4.0/grpcurl_1.4.0_linux_x86_64.tar.gz
    ```

3. Extract it and copy it to a directory that is in your `PATH`
   environment variable, e.g.:

    ```bash
    unar grpcurl_1.4.0_linux_x86_64.tar.gz
    sudo cp grpcurl_1.4.0_linux_x86_64/grpcurl /usr/local/bin
    ```

4. Change the current directory to one where gRPC protocol files can
   be found, e.g., in a Fencer repository directory:

    ```bash
    cd fencer
    ```

5. To submit a request to a running gRPC server (be it Ratelimit or
   Fencer) and consequently reduce the available amount of requests to
   be made, run:

    ```bash
    grpcurl -proto proto/rls.proto -plaintext -d '{"domain":"basic", "descriptors":[{"entries":[{"key":"key1"}]}]}' localhost:8081 envoy.service.ratelimit.v2.RateLimitService.ShouldRateLimit
    ```

   If everything went fine, you should get an OK response informing
   you of the limit remaining.
