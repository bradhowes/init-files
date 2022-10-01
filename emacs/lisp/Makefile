CURRENT_DIR := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

all:
	@echo $(CURRENT_DIR)
	touch *.el
	emacs --batch -L . -L --eval "(byte-recompile-directory \"$(CURRENT_DIR)\" 0 t nil)"
