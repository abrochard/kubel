# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]
- remove deprecated function
- copy last command ran to clipboard
- ability to mark a resource to delete
- `q` to exit resource details (`quit-window` or `kill-buffer`)
- show diff and confirmation before applying a change
- easily get logs for a previous pod instance
- log for multiple pods or whole service/deployment
- top po command + sort by usage
- safe way to help with rollbacks?

## [3.0.0] -
### Added
- tailing logs of init container
- exec directly with shell & eshell
- "evicted" state to status list
- kubel process buffer (bound to `$`) to log executed `kubectl` commands and exit codes with errors
- `kubel--exec-to-string` to replace `shell-exec-to-string` and log to process buffer
- can mark multiple items and perform bulk action:
  - delete multiple items of the same resource (like multiple pods)
  - tail logs of multiple pods
  - jab multiple deployments

### Changed
- `e` keybinding now opens a popup for exec options
- `kubel--exec`  is now completely async

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
