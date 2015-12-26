# smartwin-mode
[![License GPL 2](https://img.shields.io/badge/license-GPL_2-green.svg)](http://www.gnu.org/licenses/gpl-2.0.txt)
[![MELPA](http://melpa.org/packages/smartwin-badge.svg)](http://melpa.org/#/smartwin)

A minor mode for emacs to show shell like buffers, when smartwin-mode is on,
open a shell will use specific window to show it. The window is always at the
bottom, and can shrink or enlarge automaticlly when move in or out it.

## Installation

If you would like to install the package manually, download or clone it and put
on Emacs' `load-path`, then you can require it in your init file like this:

    (require 'smartwin)

It's available via MELPA, so you can just <kbd>M-x package-install RET smartwin
RET</kbd>.

## Usage

Add this to .emacs:

    (smartwin-mode 1)

or run <kbd>M-x smartwin-mode</kbd> to toggle it manually.

To switch between buffers of smart window, you can bind keys like:

    (define-key smartwin-mode-map (kbd "C-c s") 'smartwin-switch-buffer)

smartwin provides a function to clear shell like buffer like `clear` command:

    (define-key smartwin-mode-map (kbd "C-l") 'smartwin-clear-shell)

Then try run <kbd>M-x shell or eshell</kbd>.

## Customization

If you want to show more buffers in smart window, please customize variable
`smartwin-buffers` or group `smartwin`.

## License

The idea of `smartwin` is from ECB's `compile-window`, and initial work is based
on `popwin`, thanks to all authors of them, and author of `popwin` is included
in the author.

Copyright (C) 2015 GuanghuiXu

Copyright (C) 2011-2015 Tomohiro Matsuyama

Distributed under GNU GPL, version 2.
