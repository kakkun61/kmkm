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

.PHONY: clean
clean:
	-$(PWSH) -Command "Remove-Item -Recurse out"
	-$(MAKE) -C compiler clean
	-cd editor/vscode && npm run clean
