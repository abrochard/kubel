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
- describe and live edit a resource (deployment,service,job,ingress,configmap)
- show rollout history for a resource
- highlight a pod by name

## Installation

Get it from Melpa, or copy and load the `kubel.el` file.

If you want to have the evil compatibility package, get it from Melpa as well or
load the `kubel-evil.el` file.

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
f => set a substring filter for pod name
r => see the rollout history for resource
```

# Editing a resource

After describing a resource in YAML format, you can edit the buffer and hit `C-c C-c` to apply your changes.

For example, if you want to edit a deployment, you can hit the key sequence `d -y d`, then select your deployment, edit the buffer, and hit `C-c C-c` to apply.

## Customize

By default, kubel log tails from the last 100 lines, you can change the `kubel-log-tail-n` variable to set another line number.


## TODO
- [ ] HPA
- [ ] Endpoints
- [ ] Accounts
- [ ] Secrets
- [ ] logs for previous instance
- [ ] logs for multiple pods
- [ ] top po command + sort by usage
- [ ] tramp like
- [ ] rollback
