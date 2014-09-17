EMACS_INSTAL=emacs/emacs-pkg-install.sh
EMACS_BASIC=$(CURDIR)/emacs/emacs-basic

.PHONY : emacs-basic usage

usage :
	echo "Available targets:"
	echo "      - emacs-basic"

emacs-basic :
	${EMACS_INSTAL} fill-column-indicator
	${EMACS_INSTAL} magit
	${EMACS_INSTAL} multiple-cursors
	ln -s ${EMACS_BASIC} ${HOME}/.emacs
