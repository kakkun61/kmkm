PWSH = pwsh
INSTALL_DIR = $(APPDATA)\kmkm

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

.PHONY: install
install: $(INSTALL_DIR)
	$(MAKE) -C compiler install INSTALL_DIR=$(INSTALL_DIR)\bin
	$(PWSH) -Command "Copy-Item -Recurse library $(INSTALL_DIR)"

$(INSTALL_DIR):
	$(PWSH) -Command "New-Item -ItemType Directory -Path $(INSTALL_DIR)"

.PHONY: clean
clean:
	-$(PWSH) -Command "Remove-Item -Recurse out"
	-$(MAKE) -C compiler clean
	-cd editor/vscode && npm run clean
