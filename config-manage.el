;;; config-manage.el --- Manage abstract configurations  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 - 2020 Paul Landes

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: internal maint
;; URL: https://github.com/plandes/buffer-manage
;; Package-Requires: ((emacs "26"))
;; Package-Version: 0

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

(require 'config-manage-declare)

(defalias 'config-manage-mode-assert 'config-manage-declare-mode-assert)
(defalias 'config-manage-slots 'config-manage-declare-slots)

(require 'config-manage-base)
(require 'config-manage-prop)
(require 'config-manage-mode)

(defconst config-manager-status-defs config-manage-base-status-defs)
(defalias 'config-manager-iterate-name 'config-manage-base-iterate-name)
(defalias 'config-manager-add-entry 'config-manage-base-add-entry)
(defalias 'config-manage-mode-refresh 'config-manage-base-mode-refresh)
(defalias 'config-manage-refresh-windows 'config-manage-base-refresh-windows)

(provide 'config-manage)

;;; config-manage.el ends here
