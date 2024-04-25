CURRENT_DIR := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

all:
	@echo $(CURRENT_DIR)
	rm -rf *.elc
	emacs --batch \
	  --eval '(package-initialize)' \
	  --eval '(setq load-path (append (list (expand-file-name "~/.emacs.d/lisp")) load-path))' \
	  --eval '(message "%s" load-path)' \
	  --eval '(byte-recompile-directory "$(CURRENT_DIR)" 0 t nil)'
