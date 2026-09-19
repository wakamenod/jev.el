# macOS installs from emacs-plus keep the binary inside the app bundle and
# off PATH; fall back to the newest one rather than failing with "not found".
EMACS ?= $(shell command -v emacs 2>/dev/null || \
           ls -d /Applications/Emacs.app/Contents/MacOS/Emacs \
                 /opt/homebrew/Cellar/emacs-plus*/*/Emacs.app/Contents/MacOS/Emacs \
                 2>/dev/null | tail -1)
ELS = jev-core.el jev-providers.el jev-http.el jev-usage.el jev.el

.PHONY: all compile test demo clean

all: compile test

compile:
	$(EMACS) -Q -batch -L . --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $(ELS)

# load-prefer-newer: never run the suite against a .elc left behind by an
# earlier compile of a file that has since changed.
test:
	$(EMACS) -Q -batch --eval "(setq load-prefer-newer t)" -L . -L tests \
	  -l tests/jev-tests.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc

demo:
	$(EMACS) -Q -batch --eval "(setq load-prefer-newer t)" -L . \
	  $(if $(PROVIDER),--eval "(setq jev-provider '$(PROVIDER))") \
	  -l examples/triage.el
