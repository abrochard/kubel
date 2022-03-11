[![MELPA](https://melpa.org/packages/kubel-badge.svg)](https://melpa.org/#/kubel)
[![abrochard](https://circleci.com/gh/abrochard/kubel.svg?style=svg)](https://app.circleci.com/pipelines/github/abrochard/kubel)

# kubel

Emacs extension for controlling Kubernetes with limited permissions.

![screenshot](screenshot.png)

You can [watch how kubel started](https://www.youtube.com/watch?v=w3krYEeqnyk) or [read about it](https://gist.github.com/abrochard/dd610fc4673593b7cbce7a0176d897de).

## Features
We now support managing pretty much any resource!

- Switch context and namespace.
- Show any resource (pods/services/deployments/etc).
- Highlight a resource by name.
- Copy resource name to clipboard.
- Show and edit resource details.
- Show rollout history for a resource.
- Delete a resource.
- Tail container logs (possibly with `-f` follow flag).
- Copy container log command to clipboard.
- Port forward a pod to your localhost.
- Exec into a pod using tramp.
- Multiple kubel buffers, each one with different context, namespace, and resource.
- Quick run shell-command
- Scale replicas

## Installation

Get it from Melpa, or copy and load the `kubel.el` file.

If you want to have the evil compatibility package, get it from Melpa as well or
load the `kubel-evil.el` file.

## Usage

### `M-x kubel`

Call `kubel` to open a new kubel buffer. Call `kubel` again to start a new
session for a different context/namespace/resource.

Each kubel buffer will automatically be renamed using the following template:
```
*kubel session:  |<context>|<namespace>|<resource>|*
```

### `M-x kubel-refresh`

Call `kubel-refresh` or hit `g` (`x` in evil-mode) to refresh the current kubel buffer using the
configured context/namespace/resource for that session.

### `M-x kubel-open`

Call `kubel-open` to programmatically open a new session for the passed context,
namespace, and resource.

```lisp
(kubel-open "<context>" "<namespace>" "<resource>*kubel session:  |management-deploys-us-east1-2|gkrane-jobs|Pods|*")
```

### Changing context, namespace, and resource for a kubel session

To change the context, namespace, or resource for a session, call:
- `M-x kubel-set-context` or hit `C`.
- `M-x kubel-set-namespace` or hit `n`.
- `M-x kubel-set-resource` or hit `R`.

Note that context will autocomplete but not necessarily namespaces depending on
your permissions and cluster setup.  See the [customize section](#Customize) on
how to tune `kubel-use-namespace-list`.

## Shortcuts

On the kubel screen, place your cursor on a resource
```
enter => get resource details
C-u enter => describe resource
h => help popup
? => help popup
! => quick run shell-command
E => quick edit any resource
g => refresh (`x` in evil-mode)
k => delete popup
r => see the rollout history for resource
p => port forward pod
l => log popup
e => exec popup
j => jab deployment to force rolling update
S => scale resource replicas
C => set context
n => set namespace
R => set resource
K => set kubectl config file
F => set output format
f => set a substring filter for resource name
M-n => jump to the next highlighted resource
M-p => jump to previous highlighted resource
m => mark item
u => unmark item
M => mark all items
U => unmark all items
c => copy popup
$ => show process buffer
s => show only resources with specified label value
```

## Editing a resource

After describing a resource in YAML format, you can edit the buffer and hit `C-c C-c` to apply your changes.

For example, if you want to edit a deployment, you can switch the resource with `R` and select "Deployments", then press return with the cursor on the row you wish to edit, edit the buffer, and hit `C-c C-c` to apply.

Alternatively, you can hit `E` to then select the resource type and the resource name of what you want to edit.

## Customize

- By default, kubel log tails from the last 100 lines, you can change the `kubel-log-tail-n` variable to set another line number.
- If you need to switch or set your kubectl config file by setting your `KUBECONFIG` environment variable, you can use the wrapper function `kubel-set-kubectl-config-file` or the `K` shortcut.
- Namespace listing for auto-completion is controlled by `kubel-use-namespace-list`:
  - auto - default, use `kubectl auth can-i list namespace` to determine if we can list namespaces
  - on - always assume we can list namespaces
  - off - always assume we cannot list namespaces

## Releases

See the [CHANGELOG.md](CHANGELOG.md) for the list of changes and maybe upcoming features.
