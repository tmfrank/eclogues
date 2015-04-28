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

### DELETE `/job/:name`
Kill a job.

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
    }
}
```

Known Issues
------------
Job names need to be unique, or statuses may not update correctly.
