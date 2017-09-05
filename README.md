# kuberlnetes

An API library for Kubernetes wrapping the authentication setup. The [Swaggerl](https://github.com/philipcristiano/swaggerl) library must be used to interact with Kubernetes.

## Example

```
API = kuberlnetes:load().
swaggerl:op(API, "listCoreV1Namespace", []).
```

## TODO

At the moment this supports a local instance of minikube. Basic things still need to be done

- [ ] Support multiple contexts
- [ ] Support running from within a Kubernetes cluster
- [ ] Verify Kubernetes CA
