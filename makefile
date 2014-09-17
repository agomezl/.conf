.PHONY : emacs-basic usage

usage :
	echo "Available targets:"
	echo "      - emacs-basic"

emacs-basic :
	make -C emacs/
