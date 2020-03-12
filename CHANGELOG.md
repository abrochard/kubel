# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]
- show diff and confirmation before applying a change
- easily get logs for a previous pod instance
- log for multiple pods or whole service/deployment
- top po command + sort by usage
- safe way to help with rollbacks?

## [2.0.0] - 2020-03-XX
### Added
- show any resource with some color coding
- resource selector with kubectl version awareness
- context based functions (log, port-forward, etc)
- quick edit any resource from any context
- can set output format
- can delete any resource
- jump to next and previous highlighted resource shortcuts

### Changed
- limited resource editing capability to work with the resource selecting capability

### Removed
- `d` shortcut to edit a resource

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
