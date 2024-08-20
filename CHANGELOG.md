# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## 0.3.8 - 2024-08-20
### Fixed
-  Fixed problems in ClojureScript parsing floats with trailing decimal points.

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

[Unreleased]: https://github.com/quoll/raphael/compare/0.3.8...HEAD
[0.3.8]: https://github.com/quoll/raphael/compare/0.3.7...0.3.8
[0.3.7]: https://github.com/quoll/raphael/compare/0.1.0...0.3.7

