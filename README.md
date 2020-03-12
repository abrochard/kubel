[![MELPA](https://melpa.org/packages/kubel-badge.svg)](https://melpa.org/#/kubel)

# kubel

Emacs extension for controlling Kubernetes with limited permissions.

## Features
We now support managing pretty much any resource!

- switch context and namespace
- show any resource (pods/services/deployments/etc)
- highlight a resource by name
- copy resource name to clipboard
- show and edit resource details
- show rollout history for a resource
- delete a resource
- tail container logs (possibly with `-f` follow flag)
- copy container log command to clipboard
- port forward a pod to your localhost
- exec into a pod using tramp

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

To switch to showing a different resource, use the `R` command or
```
M-x kubel-set-resource
```
This will let you select a resource and re-display the kubel buffer.

## Shortcuts

On the kubel screen, place your cursor on a resource
```
enter => get resource details
h => help popup
C => set context
n => set namespace
R => set resource
F => set output format
g => refresh
f => set a substring filter for resource name
M-n => jump to the next highlighted resource
M-p => jump to previous highlighted resource
E => quick edit any resource
r => see the rollout history for resource
l => log popup
c => copy popup
k => delete popup
e => exec into pod
p => port forward pod
j => jab deployment to force rolling update
```

## Editing a resource

After describing a resource in YAML format, you can edit the buffer and hit `C-c C-c` to apply your changes.

For example, if you want to edit a deployment, you can switch the resource with `R` and select "Deployments", then press return with the cursor on the row you wish to edit, edit the buffer, and hit `C-c C-c` to apply.

Alternatively, you can hit `E` to then select the resource type and the resource name of what you want to edit.

## Customize

By default, kubel log tails from the last 100 lines, you can change the `kubel-log-tail-n` variable to set another line number.

## Releases

See the [CHANGELOG.md](CHANGELOG.md) for the list of changes and maybe upcoming features.
