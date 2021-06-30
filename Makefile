PWSH = pwsh

.PHONY: all
all: compiler vscode

.PHONY: compiler
compiler:
	$(MAKE) -C compiler

.PHONY: vscode
vscode:
	cd editor/vscode && npm run package

.PHONY: test
test:
	$(PWSH) -File test/run.ps1
