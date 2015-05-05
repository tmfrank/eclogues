hs-aurora-thrift
================

```
cabal build
dist/build/aurora-thrift/aurora-thrift
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
Clean up a non-terminated job.

### POST `/create`
Post a task spec of the form:

```
{
    "name": "hello",
    "command": "/bin/echo hello world",
    "resources": {
        "disk": "10",
        "ram": "10",
        "cpu": "0.1"
    },
    "dependson": []
}
```

Known Issues
------------
Job names need to be unique, or statuses may not update correctly.
