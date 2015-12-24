# smartwin-mode
A minor mode for emacs to show shell like buffers, when smartwin-mode is on, open a shell will use specific window to show it. The window is always at the bottom, and can shrink or enlarge automaticlly when move in or out it.

To use smartwin, place this file to load path and add this to .emacs:

    (require 'smartwin)
    (smartwin-mode 1)

or run M-x smartwin-mode to toggle it.

To switch between buffers of smart window, you can bind keys like:

    (define-key smartwin-mode-map (kbd "C-c r") 'smartwin-switch-buffer)

Then try run M-x shell or or eshell, if you want to show more buffers in smart
window, please customize variable `smartwin-buffers` or group `smartwin`.
