# smartwin-mode
A minor mode for emacs to show shell like buffers.

To use smartwin, place this file to load path and add this to .emacs:

(require 'smartwin)
(smartwin-mode 1)
or run M-x smartwin-mode to toggle it.

Then try run M-x shell or or eshell, if you want to show more buffers in smart
window, please customize variable: smartwin-buffers.
