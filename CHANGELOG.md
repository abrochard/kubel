# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]
- more unit tests
- `q` to exit resource details (`quit-window` or `kill-buffer`)
- show diff and confirmation before applying a change
- easily get logs for a previous pod instance
- top po command + sort by usage
- safe way to help with rollbacks?
- run `kubectl describe pod`
- `kubectl` apply a buffer to current context/namespace

## [3.1.0] -
### Added
- Support for multiple kubel buffers, each one with different context, namespace, and resource
- Support for customization of shell buffer names
- `kubel-kill-buffer` command to cancel an edit buffer without applying changes
- Support for "wide" list view
- Support for `eat` terminal emulator
- `kubel-set-namespace` support for optional parameter to refresh the namespace cache
- Custom default namespace `kubel-default-namespace` with default to `"default"`

### Changed
- Remove suffix "(default)" when acting on resources of StorageClasses
- Better parsing of the kubectl version string via JSON
- Fix describe resources not going to top of buffer
- using -- for all kubectl exec uses
- Fix `kubel--exec` doesn't work on remote systems
- Fix `dir-prefix` for TRAMP without explicitly specifically the username
- vterm sessions to use their own buffers
- case consistent resource names ("pods)
- Fix: don't assume that kubectl will use the namespace default implictely

### Removed
- `kubel-kubernetes-version` no longer cares about version patch
- support for kubernetes v1.13 and below
- support for emacs 25 and below

## [3.0.0] - 2022-09-09
### Added
- tailing logs of init container
- exec directly with shell & eshell
- "evicted" state to status list
- kubel process buffer (bound to `$`) to log executed `kubectl` commands and exit codes with errors
- `c C` to copy last command ran to clipboard
- `kubel--exec-to-string` to replace `shell-exec-to-string` and log to process buffer
- can mark multiple items and perform bulk action:
  - delete multiple items of the same resource (like multiple pods)
  - tail logs of multiple pods
  - jab multiple deployments
- `s` command to filter by selector
- added context to fetch api-resources
- support selecting container on exec function
- `kubel-exec-pod-by-shell-command` by using shell-command we can quickly run a command
- support multiple words column headers (and fix output parsing for cronjobs.batch)
- `kubel-scale-replicas` to scale a deployment, replica set, replication controller, or stateful set
- proper sort by duration for "AGE", "DURATION", and "LAST SCHEDULE" columns
- vterm support for `kubectl exec`
- ansi-term support for `kubectl exec`
- `kubel-open` function to programatically open to a specific context/namespace/resource

### Changed
- `e` keybinding now opens a popup for exec options
- `kubel--exec` is now completely async
- support selecting container on exec function
- pod label parsing updated for newer version of k8s
- empty space "" from showing up in selection candidates
- fix position calculations in kubel--parse-body
- string regex to ignore warning on `can-i` list namespace
- update transient's definition macros by their new names
- use `kubel-kubectl` variable to find `kubectl` path rather than hardcode it
- kill kubel buffer after prompting for new the context
- changed kubel-exec-vterm-pod to use to /bin/sh for stability
- defcustom :type entries
- status color is now derived from theme
- better stderr handling for context selection
- better line tracking between refreshes using tabulated-list-mode
- vterm dep is now optional

### Removed
- remove deprecated function bound to `d`
- use of `goto-line` outside of interactive use

## [2.1.0] - 2020-11-16
### Added
- CI tests
- dynamically list namespaces if permissions allow for it
- function to set the kubeconfig
- caching of kubectl version & resource lists
- support to operate remote cluster via TRAMP
- support to tail logs by pod label
- port forwarding prompt supports local:container port format

### Changed
- log buffer is readonly by default
- ask y-or-n before applying resource
- re-use same window when switching namespace

## [2.0.0] - 2020-03-12
### Added
- show any resource with some color coding
- resource selector with kubectl version awareness
- context based functions (log, port-forward, etc)
- quick edit any resource from any context
- can set output format
- can delete any resource
- jump to next and previous highlighted resource shortcuts
- save line position before refreshing so cursor doesn't jump back to top

### Changed
- limited resource editing capability to work with the resource selecting capability

### Removed
- `d` keybinding to edit a resource

## [1.0.0] - 2020-02-22
### Added
- show pods with color coding
- multiple context & namespaces with limited permissions
- copy to clipboard functions
- pod logs functions
- port forward
- rollout history
- tramp exec
- some resource editing capabilities
- pod deletion
- regex based filter for highlighting a row
