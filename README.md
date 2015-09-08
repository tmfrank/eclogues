Eclogues
========
[![Build status](https://img.shields.io/circleci/project/rimmington/eclogues/master.svg)](https://circleci.com/gh/rimmington/eclogues/tree/master)

*The Eclogues (/ˈɛklɒɡz/; Latin: Eclogae [ˈɛklɔɡaj]), also called the Bucolics, is the first of the three major works of the Latin poet Virgil.*

Installing with [Stack](https://github.com/commercialhaskell/stack)
-------------------------------------------------------------------

```
sudo apt-get install libzookeeper-mt-dev
git clone git@github.com:rimmington/eclogues
cd eclogues
stack install
```

Project Dependencies
--------------------

Requires Apache [Mesos](http://mesos.apache.org) and [Aurora](http://aurora.apache.org/) 0.7.0.

The [thrift](http://hackage.haskell.org/package/thrift) dependency does not build
due to [a bug in the .cabal file](https://issues.apache.org/jira/browse/THRIFT-3003),
and has some [interop issues](https://issues.apache.org/jira/browse/THRIFT-3145).
Clone it from [the GitHub mirror](https://github.com/apache/thrift) and apply the
included `thrift-fixes.patch` before `cabal install`.

Configuration
-------------

```
# cat MASTER_MACHINE/etc/xdg/eclogues/api.json
{
    "jobsDir": "SHARED_JOBS_DIR",
    "zookeeperHosts": "ZOOKEEPER:2181,ADDRESSES:2181",
    "bindAddress": "0.0.0.0",
    "bindPort": 8000,
    "subexecutorUser": "vagrant",
    "outputUrlPrefix": "http://localhost:9000/",
    "graphiteUrl": "http://localhost:8888/"
}
```

```
# cat SLAVE_MACHINE/etc/xdg/eclogues/subexecutor.json
{"jobsDir":"SHARED_JOBS_DIR"}
```

```
# cat SLAVE_MACHINE/$XDG_DATA_HOME/eclogues/client.json
{"zookeeperHosts":"ZOOKEEPER:2181,ADDRESSES:2181"}
```

Running
-------

```
stack build
export DIST_DIR=$(stack path --local-install-root)/bin
cp $DIST_DIR/eclogues-subexecutor $DIST_DIR/eclogues-client SLAVE_PATH
$DIST_DIR/eclogues-api &
xdg-open http://localhost:8000
```

API
---

### GET `/jobs`
List all jobs.

### GET `/jobs/:name`
Information about a particular job.

### GET `/jobs/:name/stage`
Get only the stage of a particular job.

### PUT `/jobs/:name/stage`
Kill a job, by putting the following stage:

```
{"type":"Failed","reason":"UserKilled"}
```

### GET `/jobs/:name/scheduler`
Redirect to the job page on Aurora.

### GET `/jobs/:name/output?path=`
Redirect to job output. The path must be absolute, where the root is the job
working directory, eg. `/hello.txt`.

### DELETE `/jobs/:name`
Clean up a terminated job.

### POST `/jobs`
Post a job spec of the form:

```
{
    "name": "hello",
    "command": "echo hello world > hello.txt",
    "resources": {
        "disk": "10",
        "ram": "10",
        "cpu": "0.1",
        "time": "5"
    },
    "outputfiles": ["hello.txt"],
    "capturestdout": false,
    "dependson": []
}
```

Job Dependencies
----------------

The `dependson` key in the job spec contains a list of already existing jobs.
The created job will not run until each of the specified jobs have successfully
finished. The output files from dependencies are available under the
`./virgil-dependencies/JOB_NAME/` directory when a job runs.
