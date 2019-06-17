# Manager Emacs Buffers

[![MELPA badge][melpa-badge]][melpa-link]
[![MELPA stable badge][melpa-stable-badge]][melpa-stable-link]
[![Travis CI Build Status][travis-badge]][travis-link]

Provides support to multiple managed buffers of any kind.  This is helpful for
using multiple inferior shells, multiple SQL session buffers or any other piped
process that requires multiple buffers.

The library includes support for:
* A major mode and buffer for listing, switching, and organizing multiple Emacs
  buffers.
* Fast switching with customized key bindings through the customize framework.
* Switch between managers providing the same key bindings for buffer entries
  with the same key bindings for creation, switching and managing.
* Create your own trivial implementations in minimal set of Emacs Lisp code.
* Interact with buffer entries and manager as objects with a straight forward
  API.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
## Table of Contents

- [Usage](#usage)
- [Extending Packages](#extending-packages)
- [Entries Mode](#entries-mode)
    - [Key Bindings](#key-bindings)
- [Fast Switching](#fast-switching)
- [Key Bindings](#key-bindings-1)
- [Extending Libraries](#extending-libraries)
- [Implementations](#implementations)
- [Changelog](#changelog)
- [License](#license)

<!-- markdown-toc end -->


## Usage

This isn't useful by itself, you need to extend but subclassing eieio (Emacs
Lisp) objects.  See the [buffer shell] project for an example.  This is done by
extending Emacs `eieio` objects and can be done in ~90 lines of code (example:
[buffer shell]).

To create your own managed buffer set you must extend `buffer-entry` that
defines behavior for each created managed buffer.  You must also extend the
manager itself `buffer-manager`.


## Extending Packages

This library on its own isn't useful.  It provides a lot of behavior for
packages that extend it (called *extensions*).  An example of an extension is
the [buffer shell] library and the iSQL library (to be made public soon).

When extending a package the `buffer-manage` library generates several
functions and creates keybindings to those functions as described in the
[entries mode] and [key bindings] sections.  For this documentation
commands/functions that are created are notated as `<package name>-<rest of
function name>`.  For example, to create a new shell with the `bshell` package,
you'd use `M-x bshell-new`.


## Entries Mode

Any package that extends `buffer-manage` and registered is put in a list and
can be switched with `C-x C-;` (see [key bindings]) or `M-x
buffer-manager-bind-functions`.  After you've chosen a buffer manage extension
you can then manage those buffers by with `C-tab` or `M-x <package name>-list`.
This mode provides a list of live buffers and (optionally) their working
directory.


### Key Bindings

The [entries mode] buffer allows for mouse editing and also the following keys:

|              Key | Function                           | Description                                                         |
|-----------------:|------------------------------------|---------------------------------------------------------------------|
|              `d` | config-manage-mode-mark-delete     | Delete a buffer (terminate).                                        |
|              `G` | config-manage-mode-refresh         | Refresh the buffer entry listing buffer.                            |
|              `i` | config-manage-mode-new             | Create a new entry.                                                 |
|              `n` | config-manage-mode-next            | Called by pressing the `tab` key in `config-manage-mode`.           |
|              `p` | config-manage-mode-previous        | Called by pressing the `tab` key in `config-manage-mode`.           |
|              `q` | config-manage-mode-quit            | Quit from within the `config-manage-mode`.                          |
|              `r` | config-manage-mode-rename          | Rename a buffer to NEW-NAME.                                        |
|              `s` | config-manage-mode-mark-show       | Display (show) a buffer.                                            |
|              `u` | config-manage-mode-mark-undelete   | Unmark a buffer for deletion.                                       |
|              `v` | config-manage-mode-view            | Activates the buffer entry with name NAME.                          |
|              `x` | config-manage-mode-delete-selected | Delete all entries that are selected for delete.                    |
|              `e` | config-manage-mode-edit            | Edit (configure) the entry (meaningful to [flexible configuration]) |
|              `?` | config-manage-mode-info            | Get information on the configuration entry.                         |
|       `<C-down>` | config-manage-mode-next            | Called by pressing the `tab` key in `config-manage-mode`.           |
|         `<C-up>` | config-manage-mode-previous        | Called by pressing the `tab` key in `config-manage-mode`.           |
| `<down-mouse-2>` | config-manage-mode-mouse-down      | Call back for mouse down events.                                    |
|      `<mouse-2>` | config-manage-mode-mouse-up        | Call back for mouse down events.                                    |
|       `<return>` | config-manage-mode-activate-buffer | Activates the buffer entry with name NAME.                          |


## Fast Switching

To easily and quickly switch between modes, you can use `C-x C-h` or `M-x
<package name>-switch`.  Using this command when there are no buffer entries
creates one.  Switching by buffer entry name completion is invoked by adding
the universal command (`C-u C-x C-h`) on invocation.  Currently, there are two
ways to cycle through buffers:
* *last-visit*: go to the last visited buffer entry
* *next*: go to the next highest priority buffer entry

To alternate between these cycling modes use `C-x C-'` or `M-x <package
name>-toggle-cycle-method`.

**Important:** To get the fast switching or any keybinding support (not
including the the [entries mode] buffer).


## Key Bindings

The customize system persists the key bindings for all buffer managers.  You
can view or modify the buffer management functions with: `M-x
customize-variable buffer-manage-key-bindings`.  You'll see other registered
buffer manager extensions in for this customized variable.


## Extending Libraries

Extend `buffer-manage.el` if you need to manage many separate buffers.
However, if you need just a simple persistence or configuration management
(without buffer management support) you can `require` the `config-manage.el`
library instead.  See for [frame customize] project as an example of how to
extend the *configuration manager*.


## Implementations

Implementations that use this library include (please let me know of your own):

* The [flexible configuration] library config-manage-mode-info.
* Multiple [buffer shell] library.
* The Emacs SQL [ciSQL](https://github.com/plandes/cisql) library.


## Changelog

An extensive changelog is available [here](CHANGELOG.md).


## License

Copyright © 2017 - 2019 Paul Landes

GNU Lesser General Public License, Version 2.0


<!-- links -->

[entries mode]: #entries-mode
[buffer shell]: https://github.com/plandes/bshell
[key bindings]: #key-bindings
[frame customize]: https://github.com/plandes/frame-customize
[flexible configuration]: https://github.com/plandes/flex-compile

[melpa-link]: https://melpa.org/#/buffer-manage
[melpa-stable-link]: https://stable.melpa.org/#/buffer-manage
[melpa-badge]: https://melpa.org/packages/buffer-manage-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/buffer-manage-badge.svg
[travis-link]: https://travis-ci.org/plandes/buffer-manage
[travis-badge]: https://travis-ci.org/plandes/buffer-manage.svg?branch=master
