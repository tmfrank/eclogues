Eclogues
========

*The Eclogues (/ˈɛklɒɡz/; Latin: Eclogae [ˈɛklɔɡaj]), also called the Bucolics, is the first of the three major works of the Latin poet Virgil.*

Dependencies
------------

Requires Apache [Mesos](http://mesos.apache.org) and [Aurora](http://aurora.apache.org/) 0.7.0.

The [thrift](http://hackage.haskell.org/package/thrift) dependency does not build
due to [a bug in the .cabal file](https://issues.apache.org/jira/browse/THRIFT-3003),
and has some [interop issues](https://issues.apache.org/jira/browse/THRIFT-3145).
Clone it from [the GitHub mirror](https://github.com/apache/thrift) and apply the
included `thrift-fixes.patch` before `cabal install`.

Requires the master branch of [servant-pandoc](https://github.com/mpickering/servant-pandoc)
until that's published to Hackage.

Running
-------

```
cabal build
cp dist/build/eclogues-subexecutor/eclogues-subexecutor SLAVE_PATH
echo '{"jobsDir":"SHARED_JOBS_DIR"}' > SLAVE_MACHINE/etc/eclogues/subexecutor.json
dist/build/eclogues-api/eclogues-api SHARED_JOBS_DIR ZOOKEEPER:2181,ADDRESSES:2181 THIS_HOST
xdg-open http://localhost:8000
```

API
---

### GET `/jobs`
List all jobs.

### GET `/jobs/:name`
Information about a particular job.

### GET `/jobs/:name/state`
Get only the state of a particular job.

### PUT `/jobs/:name/state`
Kill a job, by putting the following state:

```
{"type":"Failed","reason":"UserKilled"}
```

### DELETE `/jobs/:name`
Clean up a terminated job.

### POST `/jobs`
Post a task spec of the form:

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

Known Issues
------------
Job names need to be unique, or statuses may not update correctly.
