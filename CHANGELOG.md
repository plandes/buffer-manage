# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).


## [Unreleased]
### Changed
- Upgraded to Emacs [zenbuild].
- Compat with recent *flycheck* and `package-lint`.
- Minor bug fixes.


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
[Unreleased]: https://github.com/plandes/buffer-manage/compare/v0.10...HEAD
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
