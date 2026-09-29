CURRENT_DIR := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

all:
	@echo $(CURRENT_DIR)
	rm -rf *.elc
	emacs --batch \
	  --eval '(package-initialize)' \
	  --eval '(push "'$(CURRENT_DIR)'" load-path)' \
	  --eval '(push "'$(CURRENT_DIR)'/key-chord" load-path)' \
	  --eval '(setq byte-compile-ignore-files (list "old"))' \
	  --eval '(message "%s" load-path)' \
	  --eval '(byte-recompile-directory "$(CURRENT_DIR)" 0 t nil)'
