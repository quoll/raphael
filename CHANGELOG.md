# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## 0.3.12 - 2025-02-03
### Error Messages
- Expanded the error messages on CURIEs containing a bad character in a prefix, to include the
  character and the codepoint for the character.

## 0.3.11 - 2025-01-26
### Dependency Updates
- RuDolF 0.2.4: this has better CIDER integration
### Test Update
- Added a new test for a reported document failure. The doc passes, but it showed up a CIDER issue.

## 0.3.10 - 2024-09-06
### Dependency Updates
- Clojure 1.12.0
- Tiara 0.3.6
- RuDolF 0.2.3

## 0.3.9 - 2024-08-22
### Fixed
- Fixed parsing of literal values to always use the generator
- Fixed parsing literal booleans to correctly pick up the next character

## 0.3.8 - 2024-08-20
### Fixed
- Fixed problems in ClojureScript parsing floats with trailing decimal points.

### Dependency Updates
- Clojure 1.11.4
- ClojureScript 1.11.132
- Tiara 0.3.5
- RuDolF 0.2.2

## 0.3.7 - 2024-08-19
### Fixed
- Parsing strings now correctly calls the generator, and does not return a string directly.
- Parsing integers at the end of a statement when there is no space separating the number from the terminating period is no longer a float.

## 0.1.0 - 2022-11-27
### Added
- Initial release

[Unreleased]: https://github.com/quoll/raphael/compare/0.3.12...HEAD
[0.3.12]: https://github.com/quoll/raphael/compare/0.3.11...0.3.12
[0.3.11]: https://github.com/quoll/raphael/compare/0.3.10...0.3.11
[0.3.10]: https://github.com/quoll/raphael/compare/0.3.9...0.3.10
[0.3.9]: https://github.com/quoll/raphael/compare/0.3.8...0.3.9
[0.3.8]: https://github.com/quoll/raphael/compare/0.3.7...0.3.8
[0.3.7]: https://github.com/quoll/raphael/compare/0.1.0...0.3.7

