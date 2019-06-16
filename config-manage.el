;;; config-manage.el --- manage abstract configurations

;; Copyright (C) 2017 - 2019 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: configuration settings persistable

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package provides abstract behavior for EIEIO classes that are meant to
;; be extended.  This library provides classes that:

;; - Provide a way to manage configuration in a simplier file persistence than
;;   the EIEIO default
;; - GUI (mode buffer) that provides an easy interface to create, read, update
;;   and destroy configuration objects that then CRUD through to disk
;;   (optionally).

;;; Code:

(require 'config-manage-core)
(require 'config-manage-base)
(require 'config-manage-prop)
(require 'config-manage-mode)

(provide 'config-manage)

;;; config-manage.el ends here
