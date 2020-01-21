My Emacs configuration

`>= 25.2, < 27`

```sh
git submodule update --init --recursive
cask install
make
export PATH="$PATH:${HOME}/.emacs.d/packages/rtags/_build/bin"
emacs
```

```
M-x skk-get
```

no network
```sh
emacs -q --load rc/base.el
```
