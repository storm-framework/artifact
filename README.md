# Artifact Accompanying the STORM Paper

- [Getting Started](getting-started.md)
- [Detailed Instructions](detailed.md)

## Docker Build

Many thanks to the anonymous artifact reviewer for providing this!

1. Turn the associated `Dockerfile` into an image 

```
$ sudo docker build -t storm .
``` 
 
2. Run a container with 

```
sudo docker run -it storm
```

