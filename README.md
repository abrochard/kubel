[![MELPA](https://melpa.org/packages/kubel-badge.svg)](https://melpa.org/#/kubel)

# kubel

Emacs extension for controlling Kubernetes with limited permissions.

## Features
This is mostly pod management for now. More may come.

- switch context and namespace
- show pods
- get pod details
- tail container logs (possibly with `-f` follow flag)
- copy pod name to clipboard
- copy container log command to clipboard
- port forward a pod to your localhost

## Installation

Get it from Melpa, or copy and load the `kubel.el` file.

## Usage

To list the pods in your current context and namespace, call
```
M-x kubel
```
To set said namespace and context, respectively call
```
M-x kubel-set-namespace
M-x kubel-set-context
```
Note that namespace will autocomplete but not context,
this is because I interact with kubernetes through a user who
does not have permissions to list namespaces.

## Shortcuts

On the kubel screen, place your cursor on the pod
```
enter => get pod details
h => help popup
C => set context
n => set namespace
g => refresh pods
p => port forward pod
e => exec into pod
d => describe popup
l => log popup
c => copy popup
k => delete pod
j => jab deployment to force rolling update
```

## Customize

By default, kubel log tails from the last 100 lines, you can change the `kubel-log-tail-n` variable to set another line number.


## TODO
- [x] Pods
- [x] Deployments
- [x] Services
- [x] Config map
- [x] Jobs
- [x] Ingress
- [ ] HPA
- [ ] Endpoints
- [ ] Accounts
- [ ] Secrets
- [x] Delete pods
- [x] View logs
- [x] Describe
- [ ] Edit
- [x] Switch clusters
- [ ] Apply manifest
- [x] exec
- [ ] logs for previous instance
