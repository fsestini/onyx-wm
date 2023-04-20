# Onyx

Onyx is a minimal tiling window manager for MacOS, written and configured in Haskell. Layouts and some data structures are taken from [xmonad](), with additional bits and pieces from [chunkwm](https://github.com/saforem2/chunkwm).

While the WM is usable, the project is still experimental and largely undocumented.

Last tested on MacOS Monterey 12.6.3.

#### Build and run

    make
    cabal run onyx-wm

#### Default keybindings

* `alt + j`: focus down
* `alt + k`: focus up
* `alt + h`: focus left
* `alt + l`: focus right
* `alt + w`: close window
* `alt + return`: send focused window to master pane
* `alt + ,`: increase no. of windows in master pane
* `alt + .`: decrease no. of windows in master pane
* `alt + m`: increase size of master pane
* `alt + n`: decrease size of master pane
* `alt + =`: equalize panes
* `alt + shift + j`: swap with window below
* `alt + shift + k`: swap with window above
* `alt + shift + h`: swap with window to the left
* `alt + shift + l`: swap with window to the right
* `alt + r`: rotate layout
* `alt + space`: change layout
* `alt + t`: tile
* `alt + u`: untile
