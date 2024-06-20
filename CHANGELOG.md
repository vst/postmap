# Changelog

## 0.0.1 (2024-06-18)


### Features

* add CLI command to init specification ([7efc808](https://github.com/vst/postmap/commit/7efc8089ae53699c680c787c73de659392ad8b4c))
* **codegen:** add rudimentary support for array column types ([41c7c31](https://github.com/vst/postmap/commit/41c7c31ae3d9fffc400358b165cbab4ae654f466))
* **codegen:** consider PKs which are also FKs ([4f98c73](https://github.com/vst/postmap/commit/4f98c73d5eb0751269525a1dc5f91943152f571f))
* **codegen:** convert Markdown documentation to Haddock ([42ba7fa](https://github.com/vst/postmap/commit/42ba7fa3832c6e0eb6d63477f193cb1a6ca9fd67))
* **codegen:** suggest some hlint rules for generated code ([c97aa42](https://github.com/vst/postmap/commit/c97aa42f72c6e1ee9ba92db986174814e4a1cfab))
* **codegen:** use newtype instead of data for single field records ([738fe11](https://github.com/vst/postmap/commit/738fe11a5e199500ea7da46d317fabac05f333e3))
* complete read-only, rudimentary TUI implementation ([b8e9e60](https://github.com/vst/postmap/commit/b8e9e6014ac63f045290ed8033e0bb7ccc6a2c50))
* format generated Haskell code with fourmolu ([a41d9c9](https://github.com/vst/postmap/commit/a41d9c9571cb809dcaab2e3c0d8899b84055870a))
* **gencode:** add `bpchar` (character) type support ([55a6ec9](https://github.com/vst/postmap/commit/55a6ec96fb48dd30e980c3035edb20b1fd5c86a4))
* haskellize record/field names in schema init sub-command ([e5bd93a](https://github.com/vst/postmap/commit/e5bd93a9170cd1fcc236e2330ea4cb1dbe6eb5ba))
* implement CLI command to prepare, dump and render diagrams ([41cac18](https://github.com/vst/postmap/commit/41cac188c33cae862e4c93e4a96fbe8fddf7f0f3))
* implement database schema introspection ([96bd654](https://github.com/vst/postmap/commit/96bd654cecd2fc6ca499a9125172a989e9f1f1c0))
* implement rudimentary Haskell code generation functionality ([a600d00](https://github.com/vst/postmap/commit/a600d00a90ff63edf6960a2933286d976d4684aa))
* integrate Release Please (with GitHub Action) ([84e3a90](https://github.com/vst/postmap/commit/84e3a90d28f82cdb8844acd4d85d1afaacf4eb58))
* mark introspected table and record as view if that is the case ([513b1cf](https://github.com/vst/postmap/commit/513b1cf5fadcbbfec4bde8023d405111f987535c))
* provide default column ordering for schema init sub-command ([09cdeda](https://github.com/vst/postmap/commit/09cdeda1b041db9dba4fb1ef200b1120e4b88200))
* rudimentary TUI implementation ([cbcfd3e](https://github.com/vst/postmap/commit/cbcfd3ea9e0728da7b4a7ba516a26935cb5660fb))
* **web:** add read-only Webapp based on hyperbole ([f4c9c79](https://github.com/vst/postmap/commit/f4c9c79ad8ebc907e564c2ef89fc472ec5b187df))


### Bug Fixes

* **codegen:** disable hlint on generated modules ([9e49390](https://github.com/vst/postmap/commit/9e49390008156c1f3ac2a7195fc2046e540b4950))
* **codegen:** do not import Control.Applicative if not necessary ([765dc01](https://github.com/vst/postmap/commit/765dc01802e285de3eb4c5abba1b9e5b544d52ad))
* **codegen:** drop unused language pragmas ([46adee1](https://github.com/vst/postmap/commit/46adee1f8b794d8991e0c0b043f2ed7d63bc7807))
* **codegen:** re-export identifiers module from top-level autogen module ([ae21301](https://github.com/vst/postmap/commit/ae213019ad66647875b67bdc1afe36b6d3a5da00))
* fix autodocodec field name casing ([3c7bb2c](https://github.com/vst/postmap/commit/3c7bb2c33e27d69dea4b9a7645733886bb923147))

## Changelog
