My Emacs configuration

`>= 25.2, < 27`

build depends
* [Cask](http://cask.readthedocs.io/en/latest/)
* [Gforth](https://savannah.gnu.org/projects/gforth/)
* [Erlang/OTP](https://www.erlang.org/)

```sh
git submodule update --init --recursive
cask install
make
export PATH="$HOME/.emacs.d/packages/rtags/_build/bin:$PATH"
emacs
```

```
M-x skk-get
```

no network
```sh
emacs -q --load rc/base.el
```
