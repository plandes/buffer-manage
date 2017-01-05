# Manager Emacs Buffers [![MELPA badge][melpa-badge]][melpa-link] [![MELPA stable badge][melpa-stable-badge]][melpa-stable-link] [![Travis CI Build Status][travis-badge]][travis-link]

  [melpa-link]: https://melpa.org/#/buffer-manage
  [melpa-stable-link]: https://stable.melpa.org/#/buffer-manage
  [melpa-badge]: https://melpa.org/packages/buffer-manage-badge.svg
  [melpa-stable-badge]: https://stable.melpa.org/packages/buffer-manage-badge.svg
  [travis-link]: https://travis-ci.org/plandes/buffer-manage
  [travis-badge]: https://travis-ci.org/plandes/buffer-manage.svg?branch=master

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


## Usage

This isn't useful by itself, you need to extend but subclassing eieio (Emacs
Lisp) objects.  See the [buffer shell](https://github.com/plandes/bshell)
project for an example.  This is done by extending Emacs `eieio` objects and
can be done in ~90 lines of code
(example: [buffer shell](https://github.com/plandes/bshell)).

To create your own managed buffer set you must extend `buffer-entry` that
defines behavior for each created managed buffer.  You must also extend the
manager itself `buffer-manager`.


## Extending Packages

This library on it's own isn't useful.  It provides a lot of behavior for
packages that extend it (called *extensions*).  An example of an extension is
the [buffer shell](https://github.com/plandes/bshell) library and the iSQL
library (to be made public soon).

When extending a package the `buffer-manage` library generates several
functions and creates keybindings to those functions as described in
the [Entries Mode](#entries-mode) and [Key Bindings](#key-bindings) sections.
For this documentation commands/functions that are created are notated as
`<package name>-<rest of function name>`.  For example, to create a new shell
with the `bshell` package, you'd use `M-x bshell-new`.


## Entries Mode

Any package that extends `buffer-manage` and registered is put in a list and
can be switched with `C-x C-;` (see [key bindings](#key-bindings)) or `M-x
buffer-manager-bind-functions`.  After you've chosen a buffer manage extension
you can then manage those buffers by with `C-tab` or `M-x <package name>-list`.
This mode provides a list of live buffers and (optionally) their working
directory.

The buffer allows for mouse editing and also the following keys:

|Key               |Function                            |Description
|-----------------:|------------------------------------|-----------------------------------------------------------
|`d`               |buffer-manage-mode-mark-delete      |Delete a buffer (terminate).                              |
|`G`               |buffer-manage-mode-refresh          |Refresh the buffer entry listing buffer.                  |
|`i`               |buffer-manage-mode-new              |Create a new entry.                                       |
|`n`               |buffer-manage-mode-next             |Called by pressing the `tab` key in `buffer-manage-mode`. |
|`p`               |buffer-manage-mode-previous         |Called by pressing the `tab` key in `buffer-manage-mode`. |
|`q`               |buffer-manage-mode-quit             |Quit from within the `buffer-manage-mode`.                |
|`r`               |buffer-manage-mode-rename           |Rename a buffer to NEW-NAME.                              |
|`s`               |buffer-manage-mode-mark-show        |Display (show) a buffer.                                  |
|`u`               |buffer-manage-mode-mark-undelete    |Unmark a buffer for deletion.                             |
|`v`               |buffer-manage-mode-view             |Activates the buffer entry with name NAME.                |
|`x`               |buffer-manage-mode-delete-selected  |Delete all entries that are selected for delete.          |
|`z`               |buffer-manage-mode-show-selected    |Show all entries in one frame that are selected.          |
|`<C-down>`        |buffer-manage-mode-next             |Called by pressing the `tab` key in `buffer-manage-mode`. |
|`<C-up>`          |buffer-manage-mode-previous         |Called by pressing the `tab` key in `buffer-manage-mode`. |
|`<down-mouse-2>`  |buffer-manage-mode-mouse-down       |Call back for mouse down events.                          |
|`<mouse-2>`       |buffer-manage-mode-mouse-up         |Call back for mouse down events.                          |
|`<return>`        |buffer-manage-mode-activate-buffer  |Activates the buffer entry with name NAME.                |


## Cycling Modes

To easily and quickly switch between modes, you can use `C-x C-h` or `M-x
<package name>-switch`.  Using this command when there are no shells creates
one.  It also has special behavior when using the universal command (`C-u`) on
invocation.  Currently, there are two ways to cycle through buffers:
* *last-visit*: go to the last visited buffer entry
* *next*: go to the next highest priority buffer entry

To alternate between these cycling modes use `C-x C-'` or `M-x <package
name>-toggle-cycle-method`.


## Key Bindings

The customize system persists the key bindings for all buffer managers.  You
can view or modify the buffer management functions with: `M-x
customize-variable buffer-manage-key-bindings`.  You'll see other registered
buffer manager extensions in for this customized variable.


## License

Copyright © 2017 Paul Landes

GNU Lesser General Public License, Version 2.0
