# kuberlnetes

An API library for Kubernetes wrapping the authentication setup. The [Swaggerl](https://github.com/philipcristiano/swaggerl) library must be used to interact with Kubernetes.

`KUBECONFIG` environment variable can be used to specify the location of the config file to read. Otherwise defaults to `$HOME/.kube/config`.

## Example

```
API = kuberlnetes:load().
swaggerl:op(API, "listCoreV1Namespace", []).
```

If you know the path of the config you would like to use and don't want to rely
on an environment variable, you can use `load_file/1`.

## TODO

At the moment this supports a local instance of minikube. Basic things still need to be done

- [ ] Support multiple contexts
- [ ] Support running from within a Kubernetes cluster
- [ ] Verify Kubernetes CA
