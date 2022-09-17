# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:
- Add `Debug` instance (#52 by @JordanMartinez)

Bugfixes:

Other improvements:

## [v7.0.0](https://github.com/purescript/purescript-tuples/releases/tag/v7.0.0) - 2022-04-27

Breaking changes:
- Update project and deps to PureScript v0.15.0 (#50 by @JordanMartinez)

New features:

Bugfixes:

Other improvements:

## [v6.0.1](https://github.com/purescript/purescript-tuples/releases/tag/v6.0.1) - 2021-04-27

Other improvements:
- Fix warnings revealed by v0.14.1 PS release (#48 by @JordanMartinez)

## [v6.0.0](https://github.com/purescript/purescript-tuples/releases/tag/v6.0.0) - 2021-02-26

Breaking changes:
- Added support for PureScript 0.14 and dropped support for all previous versions (#37, #43)
- `Data.Tuple.lookup` has been moved to `Data.Foldable.lookup` in the `purescript-foldable-traversable` package (#46)

New features:
- Added Generic instance for Tuple (#40)
- This package no longer depends on the `purescript-bifunctors`, `purescript-distributive`, `purescript-foldable-traversable`, `purescript-maybe`, `purescript-newtype`, and `purescript-type-equality` packages. Relevant instances have been moved to those packages. (#46)

Bugfixes:

Other improvements:
- Added `foldr1` and `foldl1` implementations to `Foldable1 (Tuple a)` instance and removed `fold1` (#39, #43)
- Improved and expanded documentation in the module header (#30)
- Fixed mistaken reference to `Functor` in documentation comment for `Apply` instance (#33)
- Migrated CI to GitHub Actions and updated installation instructions to use Spago (#42)
- Added a changelog and pull request template (#44, #45)

## [v5.1.0](https://github.com/purescript/purescript-tuples/releases/tag/v5.1.0) - 2018-10-28

- Added `Foldable1`, `FoldableWithIndex`, `Traversable1`, `TraversableWithIndex` instances (@MonoidMusician)

## [v5.0.0](https://github.com/purescript/purescript-tuples/releases/tag/v5.0.0) - 2018-05-23

- Updated for PureScript 0.12

## [v4.1.0](https://github.com/purescript/purescript-tuples/releases/tag/v4.1.0) - 2017-05-28

- Add `Distributive` instance (@matthewleon)

## [v4.0.0](https://github.com/purescript/purescript-tuples/releases/tag/v4.0.0) - 2017-03-26

- Updated for PureScript 0.11

## [v3.2.0](https://github.com/purescript/purescript-tuples/releases/tag/v3.2.0) - 2017-03-02

- Added `Eq1` and `Ord1` instances

## [v3.1.0](https://github.com/purescript/purescript-tuples/releases/tag/v3.1.0) - 2016-12-29

- Added type operator alias for `/\` (@damncabbage)

## [v3.0.0](https://github.com/purescript/purescript-tuples/releases/tag/v3.0.0) - 2016-10-05

- Nested tuple operations are now "open" (extensible)

## [v2.0.0](https://github.com/purescript/purescript-tuples/releases/tag/v2.0.0) - 2016-10-03

- Updated dependencies
- `Nested` tuples are now right-nested for consistency with the updates to `Either`

## [v1.0.0](https://github.com/purescript/purescript-tuples/releases/tag/v1.0.0) - 2016-06-01

This release is intended for the PureScript 0.9.1 compiler and newer.
- Updated for new `Prelude` class hierarchies

**Note**: The v1.0.0 tag is not meant to indicate the library is “finished”, the core libraries are all being bumped to this for the 0.9 compiler release so as to use semver more correctly.

## [v0.4.0](https://github.com/purescript/purescript-tuples/releases/tag/v0.4.0) - 2015-06-30

This release works with versions 0.7.\* of the PureScript compiler. It will not work with older versions. If you are using an older version, you should require an older, compatible version of this library.

## [v0.3.4](https://github.com/purescript/purescript-tuples/releases/tag/v0.3.4) - 2015-03-25

- Improvements to nested tuples (@natefaubion)

## [v0.3.3](https://github.com/purescript/purescript-tuples/releases/tag/v0.3.3) - 2015-03-25

- More updates to nested tuples (@jdegoes)

## [v0.3.2](https://github.com/purescript/purescript-tuples/releases/tag/v0.3.2) - 2015-03-24

- Rework nested tuples (@jdegoes)

## [v0.3.1](https://github.com/purescript/purescript-tuples/releases/tag/v0.3.1) - 2015-03-19

- Updated docs (@brainrape)

## [v0.3.0](https://github.com/purescript/purescript-tuples/releases/tag/v0.3.0) - 2015-02-21

**This release requires PureScript v0.6.8 or later**
- Updated `purescript-monoid` dependency

## [v0.2.3](https://github.com/purescript/purescript-tuples/releases/tag/v0.2.3) - 2014-12-01

- Updated dependencies

## [v0.2.2](https://github.com/purescript/purescript-tuples/releases/tag/v0.2.2) - 2014-11-05

- Ease pain of working with nested tuples

## [v0.2.1](https://github.com/purescript/purescript-tuples/releases/tag/v0.2.1) - 2014-09-08

- Added `Comonad`, `Extract`, `Monoid`, `Semigroup`, and `Semigroupoid` instances (@joneshf)

## [v0.2.0](https://github.com/purescript/purescript-tuples/releases/tag/v0.2.0) - 2014-08-11

- Add instances for `Lazy` (@garyb)

## [v0.1.2](https://github.com/purescript/purescript-tuples/releases/tag/v0.1.2) - 2014-06-14

- Fixed `Show` instance (garyb)

## [v0.1.1](https://github.com/purescript/purescript-tuples/releases/tag/v0.1.1) - 2014-04-25

- Removed test-related stuff (moved to core-tests)

## [v0.1.0](https://github.com/purescript/purescript-tuples/releases/tag/v0.1.0) - 2014-04-21

- Initial release
