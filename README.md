Eclogues
========

*The Eclogues (/ˈɛklɒɡz/; Latin: Eclogae [ˈɛklɔɡaj]), also called the Bucolics, is the first of the three major works of the Latin poet Virgil.*

```
cabal build
dist/build/eclogues-api/eclogues-api ./jobs ZOOKEEPER:2181,ADDRESSES:2181 THIS_HOST
xdg-open http://localhost:8000
```

API
---

### GET `/jobs`
List all jobs.

### GET `/job/:name`
Information about a particular job.

### GET `/job/:name/state`
Get only the state of a particular job.

### PUT `/job/:name/state`
Kill a job, by putting the following state:

```
{"type":"Failed","reason":"UserKilled"}
```

### DELETE `/job/:name`
Clean up a terminated job.

### POST `/create`
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
