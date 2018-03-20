My Emacs configuration

25.2+ (build depends [Cask](http://cask.readthedocs.io/en/latest/) and [Gforth](https://savannah.gnu.org/projects/gforth/))
```sh
git submodule update --init --recursive
make -k
emacs -nw
```
```
M-x skk-get
M-x irony-install-server
```

no network
```sh
emacs -q --load rc/base.el
```
