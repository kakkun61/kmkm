.PHONY: all
all: compiler vscode

.PHONY: compiler
compiler:
	$(MAKE) -C compiler

.PHONY: vscode
vscode:
	cd editor/vscode && npm run package
