EMACS ?= emacs
CASK ?= cask

MAKE_PKG := \
	packages/PG \
	packages/forth-mode \
	packages/haskell-mode \
	packages/js2-mode \
	packages/rtags/_build

BYTE_COMPILE_PKG := \
	packages/asbish/*.el \
	packages/j-mode/j-help.el \
	packages/j-mode/j-console.el \
	packages/j-mode/j-font-lock.el \
	packages/bison-mode/*.el \
	packages/emacs-mips-mode/*.el \
	packages/zscript-mode/*.el

MAKE_TARGET :=
BYTE_COMPILE_TARGET := $(patsubst %.el,%.elc,$(wildcard $(BYTE_COMPILE_PKG)))

.PHONY: all setup $(MAKE_PKG) update upgrade clean

all: setup $(MAKE_PKG) $(BYTE_COMPILE_TARGET)

setup:
	@mkdir -p backup
	cd packages/rtags && mkdir -p _build && cd _build && cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ..

$(MAKE_PKG):
	@printf "\n* $@ \n"
	@printf "==============================================================\n"
	cd $@ && $(MAKE) $(MAKE_TARGET)

%.elc: %.el
	@printf "\n* $@ \n"
	@printf "==============================================================\n"
	$(CASK) $(EMACS) -Q --batch -f batch-byte-compile $^

update:
	git pull origin master
	git submodule update --init --recursive

upgrade:
	git submodule update --remote --recursive

clean: MAKE_TARGET = clean
clean: $(MAKE_PKG)
	$(RM) $(BYTE_COMPILE_TARGET)
	rm -rf packages/rtags/_build
