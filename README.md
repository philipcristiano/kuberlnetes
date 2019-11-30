# kuberlnetes

An API library for Kubernetes wrapping the authentication setup. The [Swaggerl](https://github.com/philipcristiano/swaggerl) library must be used to interact with Kubernetes.


## Authentication

* In-cluster configuration
* Kubeconfig with exec, tested with Digital Ocean

## Example

```
API = kuberlnetes:load().
swaggerl:op(API, "listCoreV1Namespace", []).
```


### Watching resources

`kuberlnetes:watch/4`

* `callback` - fun/1 ({Type, Object})
* Loaded API Config
* {ListOp, WatchOp} as returned by `swaggerl:op`
* `[]` - I guess this should be removed :/

## TODO

At the moment this supports a local instance of minikube. Basic things still need to be done

- [ ] Support multiple contexts
- [ ] Support running from within a Kubernetes cluster
- [ ] Verify Kubernetes CA
