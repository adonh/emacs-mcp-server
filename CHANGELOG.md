# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.7.0](https://github.com/rhblind/emacs-mcp-server/compare/v0.6.0...v0.7.0) (2026-05-04)


### Features

* **emacs-tools:** isolate tool load failures per-module ([cb1c95a](https://github.com/rhblind/emacs-mcp-server/commit/cb1c95aad3dc1f61e4525ee68ba03175130affcb))
* **org-capture:** add :immediate-finish injection helper ([1ba4776](https://github.com/rhblind/emacs-mcp-server/commit/1ba477663116f4895fd07dff55817d9fd8e984af))
* **org-capture:** add template string preprocessor for %^ markers ([08a3b72](https://github.com/rhblind/emacs-mcp-server/commit/08a3b7238251a88d180109ae16877a8ca6234a63))
* **org-capture:** non-interactive template mode with %^ preprocessing and :immediate-finish injection ([076e16b](https://github.com/rhblind/emacs-mcp-server/commit/076e16bf7cfa5e930ded049e2dc0a0bb193fb187))
* **org-list-templates:** expose %^ prompt metadata per template ([152a12e](https://github.com/rhblind/emacs-mcp-server/commit/152a12e871f2a0043f41a8b6f70770d94e9790a1))
* **org-roam:** add org-roam-capture tool ([fd60af8](https://github.com/rhblind/emacs-mcp-server/commit/fd60af8c811878bdd8396f22aa70beb7feee1be6))
* **org-roam:** add org-roam-get-node tool ([14ffdc4](https://github.com/rhblind/emacs-mcp-server/commit/14ffdc41c913b9bc4c16369ea4d0f73b5b56c88d))
* **org-roam:** add org-roam-search tool ([b1c45ee](https://github.com/rhblind/emacs-mcp-server/commit/b1c45ee80ebcc490d3c7248b9b5750cf0cc8fed8))
* **org:** add node-to-alist serializer ([61b1e82](https://github.com/rhblind/emacs-mcp-server/commit/61b1e8226088cb8958d5d7458ca237213522c278))
* **org:** add org-agenda tool ([dc092d5](https://github.com/rhblind/emacs-mcp-server/commit/dc092d509127ad0704ab6ef7589f7bc914697971))
* **org:** add org-archive tool ([e55fb68](https://github.com/rhblind/emacs-mcp-server/commit/e55fb68ab66bb679377994ff47b9bab3ee598c95))
* **org:** add org-capture tool ([5eaf13c](https://github.com/rhblind/emacs-mcp-server/commit/5eaf13ce06234519b2e5077aa84ac8388a552459))
* **org:** add org-clock tool ([6420052](https://github.com/rhblind/emacs-mcp-server/commit/64200522b2e06c59581bee408be038be4d2bc8e1))
* **org:** add org-common module skeleton with defcustoms ([97e39d1](https://github.com/rhblind/emacs-mcp-server/commit/97e39d13b1461d8d958124c51ffec1f90e6def7d))
* **org:** add org-get-node tool ([25174b1](https://github.com/rhblind/emacs-mcp-server/commit/25174b101057804e49380ab460452550b685a280))
* **org:** add org-list-tags tool ([92ecb4d](https://github.com/rhblind/emacs-mcp-server/commit/92ecb4df1641cda0081dfc1944a30b0cb79027b5))
* **org:** add org-list-templates tool ([5c3cc42](https://github.com/rhblind/emacs-mcp-server/commit/5c3cc42e6755906654c956fb9116a9c32a81fc9c))
* **org:** add org-refile tool ([521b1be](https://github.com/rhblind/emacs-mcp-server/commit/521b1bec042004361f86d398e384a3327ee509c3))
* **org:** add org-search tool ([88df944](https://github.com/rhblind/emacs-mcp-server/commit/88df9445ca6a734617b3a3f6f52cc31dd9561d7b))
* **org:** add org-update-node tool ([acef958](https://github.com/rhblind/emacs-mcp-server/commit/acef958feda82eeaac5f41a2f3f0adc3259c9ac5))
* **org:** add path validation helper ([324eda1](https://github.com/rhblind/emacs-mcp-server/commit/324eda1b7da7f3b436dc47d538ef88748feb6f3d))
* **org:** add resolve-node helper ([50849af](https://github.com/rhblind/emacs-mcp-server/commit/50849af48572be776e60a32220687b4fa7705040))
* **org:** add with-node, promote-to-id, augment-description helpers ([592d4e4](https://github.com/rhblind/emacs-mcp-server/commit/592d4e4fc382d702e179f6d7b679505545afdc57))
* **org:** register org-* and org-roam-* tools in emacs-tools registry ([2db1514](https://github.com/rhblind/emacs-mcp-server/commit/2db151404675a2964faad0ec7024c00357efd203))


### Bug Fixes

* **capabilities:** Remove unimplemented capabilities to prevent client from hanging ([40b88ac](https://github.com/rhblind/emacs-mcp-server/commit/40b88acc209025bb7e7177bd27d11a9bb0078b2d))
* **capabilities:** Remove unimplemented capabilities to prevent client hangs ([9df7846](https://github.com/rhblind/emacs-mcp-server/commit/9df784625db7b4cbd20564456096df636f417cf8)), closes [#14](https://github.com/rhblind/emacs-mcp-server/issues/14)
* **org-agenda:** remove unnecessary intern call in state hash map ([ab56926](https://github.com/rhblind/emacs-mcp-server/commit/ab56926f8ce347fa84f546cf946be3401aba3f1c))
* **org-list-templates:** Handle non-serializable template values safely ([fc30aee](https://github.com/rhblind/emacs-mcp-server/commit/fc30aeefd2b860a9b70dbe9d230fa34b2b18ded6))
* **org-roam:** address PR [#18](https://github.com/rhblind/emacs-mcp-server/issues/18) review round 3 (path safety, integer coercion, unique-id lookup) ([6422def](https://github.com/rhblind/emacs-mcp-server/commit/6422def0347fbededbf811b77cb8d21b5469dfe2))
* **org-roam:** aliases/refs no longer overwrite each iteration ([dbf453f](https://github.com/rhblind/emacs-mcp-server/commit/dbf453f17de3779b5244e05a680e8e629f779073))
* **org-roam:** sync roam DB after capture so node lookup succeeds ([db14518](https://github.com/rhblind/emacs-mcp-server/commit/db14518b23c7446b31752a084a351c9faa9c92a6))
* **org:** address PR [#18](https://github.com/rhblind/emacs-mcp-server/issues/18) review comments and tighten JSON/path handling ([9e6873f](https://github.com/rhblind/emacs-mcp-server/commit/9e6873f8f7b2f9a3900029d0d64d99599b7a6495))
* **org:** address PR [#18](https://github.com/rhblind/emacs-mcp-server/issues/18) review round 2 (file-level body, auto-id, roam polish) ([6ab8c28](https://github.com/rhblind/emacs-mcp-server/commit/6ab8c289c37a8008b41a6dcc24917abad47fcdb9))
* **org:** address PR [#18](https://github.com/rhblind/emacs-mcp-server/issues/18) review round 4 (read-tool side effects, id from olp) ([eec7205](https://github.com/rhblind/emacs-mcp-server/commit/eec7205000354fb1daf0fb9cd1c9a1c5ca8d8fd7))
* **org:** address PR [#18](https://github.com/rhblind/emacs-mcp-server/issues/18) review round 5 (byte-aware truncation, doc accuracy, seq requires) ([19c8ec3](https://github.com/rhblind/emacs-mcp-server/commit/19c8ec3c4eeac120322e088c9dad4aac727344a7))
* **org:** address PR [#18](https://github.com/rhblind/emacs-mcp-server/issues/18) review round 6 (heading-only resolver, annotation honesty, arg-shape validation) ([9258eca](https://github.com/rhblind/emacs-mcp-server/commit/9258eca9c0dc9065387c3f09f5d9d6be23b563eb))
* **org:** remove dead immediate_finish branch from org-capture ([eea9f13](https://github.com/rhblind/emacs-mcp-server/commit/eea9f13ecb26d41e13c522bbfe9d023333428045))
* **org:** validate org-agenda files parameter against allowed roots ([d5ad08d](https://github.com/rhblind/emacs-mcp-server/commit/d5ad08d5c6888e7b1aacd2df820532b6b7ee2bb6))
* remove leading dot from release-please-manifest.json filename ([d80b24a](https://github.com/rhblind/emacs-mcp-server/commit/d80b24a885b7ddf08a7779ef92f90f7af6c5e922))
* use x-release-please block instead of inline ([93e9900](https://github.com/rhblind/emacs-mcp-server/commit/93e9900e53bccc3266589e7739be1f99498bd062))

## [0.6.0](https://github.com/rhblind/emacs-mcp-server/compare/v0.5.0...v0.6.0) (2026-03-31)

### Fixed
- `mcp-server-security--is-sensitive-file`: patterns using `~/` prefix (e.g. `"~/.ssh/"`) were never matched because the input path was expanded with `expand-file-name` but the patterns were not, causing the literal `~` to fail against an absolute path (issue #9)
- `mcp-server-security--is-dangerous-operation`: calling `symbol-name` on a string operation ID (e.g. `"access-sensitive-file:find-file"`) raised `wrong-type-argument: symbolp` instead of returning a result
- Sensitive file check was only applied to four functions (`find-file`, `find-file-noselect`, `view-file`, `insert-file-contents`); functions like `copy-file` and `rename-file` could still access sensitive files when placed in `mcp-server-security-allowed-dangerous-functions`

### Added
- Glob pattern support (`*`, `?`) in `mcp-server-security-sensitive-file-patterns`: patterns such as `"~/.authinfo*"` now correctly match all variants (`~/.authinfo.gpg`, `~/.authinfo.enc`, etc.) via `wildcard-to-regexp`
- Sensitive file check now covers `copy-file` (args 1 and 2), `rename-file` (args 1 and 2), `write-region` (arg 3), `append-to-file` (arg 3), `write-file` (arg 1), and `insert-file-contents-literally` (arg 1), blocking both reads from and writes to sensitive paths
- New entries in default `mcp-server-security-dangerous-functions`: `append-to-file`, `async-shell-command`, `directory-files`, `directory-files-recursively`, `insert-file-contents-literally`, `make-network-process`, `make-process`, `open-network-stream`, `setenv`, `with-temp-file`, `write-file`
- Removed entries from default `mcp-server-security-dangerous-functions` that were either non-functional or overly broad: `process-environment` (a variable, not a function; the form walker never matched it), `shell-environment` (not a standard Emacs function), `require` (too restrictive for legitimate use), `save-current-buffer` and `set-buffer` (low-level context-switching primitives with no inherent danger), `switch-to-buffer` (interactive UI command with no security impact)
- Comprehensive unit test suite for `mcp-server-security` (79 ERT tests total)
- Known limitation documented and tracked: static form walker does not recurse into `let`-binding value positions and cannot detect dynamically-constructed function names (`funcall` + `intern`); see issue #10

## [0.4.0](https://github.com/rhblind/emacs-mcp-server/compare/v0.3.0...v0.4.0) (2026-01-08)

### Added
- `get-diagnostics` tool for flycheck/flymake error reporting
- Modular tool system with self-registering plugins in `tools/` directory
- Selective tool enabling via `mcp-server-emacs-tools-enabled`
- Runtime tool filtering via `mcp-server-tools-filter` predicate
- Automatic server shutdown when Unix socket terminates unexpectedly
- Comprehensive Elisp code conventions in CLAUDE.md
- Security limitations section in README documenting blocklist bypass methods

### Changed
- Lowered minimum Emacs version from 28.1 to 27.1 (native JSON available since 27.1)
- Tools now use self-registration pattern (register on `require`)
- Converted `mcp-server-debug` and `mcp-server-default-transport` to `defcustom`
- Converted security timeouts to `defcustom` for user configuration
- Replaced `sleep-for` with `sit-for` for proper event handling in main loop
- Improved tool cleanup to preserve definitions (only resets runtime state)
- Refactored tool system into modular architecture

### Fixed
- Version mismatch between package header and `mcp-server-version` defconst
- Abstraction violation: transport now uses public `mcp-server-transport-send-raw` API
- Load-path setup for missing file variables
- Removed redundant runtime `require` statement
- Removed backward compatibility aliases causing defvar/defcustom confusion
- Removed autoload cookie from internal `mcp-server-main` function

### Removed
- Backward compatibility variable aliases in security module

## [0.3.0](https://github.com/rhblind/emacs-mcp-server/compare/v0.2.0...v0.3.0) (2026-01-07)

### Added
- Granular permission prompts with session management
- GNU General Public License v3

### Fixed
- Test runner with safeguards and retries for socket operations

## [0.2.0](https://github.com/rhblind/emacs-mcp-server/compare/v0.1.0...v0.2.0) (2026-01-06)

### Added
- Enhanced sensitive file and function protection
- Repository badges for tests, license, and Emacs versions
- Emacs 30.x to CI test matrix

### Changed
- Improved documentation with socat requirement note

## [0.1.0](https://github.com/rhblind/emacs-mcp-server/releases/tag/v0.1.0) (2026-01-01)

### Added
- Initial MCP server implementation in pure Elisp
- Unix domain socket transport
- `eval-elisp` tool for executing arbitrary Elisp expressions
- Security sandbox with dangerous function blocklist
- Permission caching per session
- Multi-client support with independent connection tracking
- Python and shell wrapper scripts for MCP client integration
- Comprehensive test suite
- Demo images for theme change and poem writing
