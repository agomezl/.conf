EMACS_INSTAL=./emacs-pkg-install.sh
EMACS_BASIC=$(CURDIR)/emacs-basic


.PHONY : emacs-basic

emacs-basic :
	${EMACS_INSTAL} fill-column-indicator
	${EMACS_INSTAL} magit
	${EMACS_INSTAL} multiple-cursors
	ln -s ${EMACS_BASIC} ${HOME}/.emacs
