# Manager Emacs Buffers

Provides support to manage buffers of any kind.  This is helpful for using
multiple inferior shells, multiple SQL session buffers or any other piped
process that requires multiple buffers.

The library includes support for:
* A major mode and buffer for listing, switching, and organizing multiple emacs
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
project for an example.  This is doen by extending Emacs `eieio` objects and
can be done in ~90 lines of code
(example: [buffer shell](https://github.com/plandes/bshell)).

To create your own managed buffer set you must extend `buffer-entry` that
defines behavior for each created managed buffer.  You must also extend the
manager itself `buffer-manager`.
