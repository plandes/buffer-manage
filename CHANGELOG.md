# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).


## [Unreleased]


## [1.1] - 2021-11-22

### Added
- Functionality to add invocation of commands for `config-prop` input types,
  which is the base class used to read a compiler property setting from the
  user.


## [1.0] - 2021-09-14
Major release as the code base has been stable for a few years.

### Changed
- Better configuration property document generation.

### Removed
- Removed build package linting to fix (overly restrictive) GitHub workflow
  CLI.


## [0.13] - 2021-09-13
### Changed
- Fix/add default support for file and directory config properties.
- Fixed history variable handling for file and directory config properties.
- Fixed infinite loop on missing slot error.

## [0.12] - 2020-12-20
### Changed
- Add back buffer switch message in `buffer-manager`.
- Add name to `cl-print-object` output `config-persistent` (see
  `eieio-object-name-string`).
- Fix (other) buffer selection.  Since Emacs 27 `let` forms no longer have an
  effect on EIEIO slot values even using `with-slots`.


## [0.11] - 2020-12-17
Major refactoring: cleaned up compilation and *package-lint* warnings.

### Changed
- Upgraded to Emacs [zenbuild].
- Compat with recent *flycheck* and `package-lint`.
- Minor bug fixes.

### Removed
- Unused function `buffer-manager-display-given-entries`.


## [0.10] - 2019-06-20
### Added
- Additional proeprty types.
### Changed
- Bug fixes to slot persistence.


## [0.9] - 2019-06-15
### Added
- New property based configuration for the `config-manage` library that allows
  for quick but robust minibuffer configuration of configuration entries.
- More `config-manage-mode` options for more versile extensions like
  `flex-comple`.

### Changed
- Compat with recent EIEIO with better Emacs OO and CLOS symantics.


## [0.8] - 2019-06-15
### Changed
- Better compatibility with Emacs 26.


## [0.6] - 2018-05-28
### Added
- `config-persistent-reset` to clear state of persistent configuration objects.


## [0.5] - 2017-09-03
### Changed
- Dependencies and headers to be adhere more to convention.


## [0.4] - 2017-08-27
### Changed
- Bug fixes: entry naming enumeration name fix.


## [0.3] - 2017-08-23
### Changed
- Bug fixes: entry naming enumeration name fix, last buffer switch back.


## [0.2] - 2017-08-06
### Changed
- Split out entry management from buffer specific to allow extending from other
  libraries.

### Added
- Add configuration save.


## [0.1] - 2017-08-06
### Added
First major release.


<!-- links -->
[Unreleased]: https://github.com/plandes/buffer-manage/compare/v1.1...HEAD
[1.1]: https://github.com/plandes/buffer-manage/compare/v1.0...v1.1
[1.0]: https://github.com/plandes/buffer-manage/compare/v0.13...v1.0
[0.13]: https://github.com/plandes/buffer-manage/compare/v0.12...v0.13
[0.12]: https://github.com/plandes/buffer-manage/compare/v0.11...v0.12
[0.11]: https://github.com/plandes/buffer-manage/compare/v0.10...v0.11
[0.10]: https://github.com/plandes/buffer-manage/compare/v0.9...v0.10
[0.9]: https://github.com/plandes/buffer-manage/compare/v0.8...v0.9
[0.8]: https://github.com/plandes/buffer-manage/compare/v0.7...v0.8
[0.7]: https://github.com/plandes/buffer-manage/compare/v0.6...v0.7
[0.6]: https://github.com/plandes/buffer-manage/compare/v0.5...v0.6
[0.5]: https://github.com/plandes/buffer-manage/compare/v0.4...v0.5
[0.4]: https://github.com/plandes/buffer-manage/compare/v0.3...v0.4
[0.3]: https://github.com/plandes/buffer-manage/compare/v0.2...v0.3
[0.2]: https://github.com/plandes/buffer-manage/compare/v0.1...v0.2
[0.1]: https://github.com/plandes/buffer-manage/compare/0c28b86...v0.1

[zenbuild]: https://github.com/plandes/zenbuild
