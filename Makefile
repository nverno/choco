emacs ?= emacs
wget  ?= wget
curl  ?= curl

.PHONY: test clean distclean
all: test
test: tag-utils.el
	$(emacs) -Q -batch -L . -l ert -l test/choco-tests.el                    \
	-f ert-run-tests-batch-and-exit

README.md: el2markdown.el choco.el
	$(emacs) -batch -l $< choco.el -f el2markdown-write-readme
	$(RM) $@~

.INTERMEDIATE: tag-utils.el
tag-utils.el:
	$(curl) -fsSkL --retry 9 --retry-delay 9                                 \
	"https://raw.githubusercontent.com/nverno/tag-utils/master/tag-utils.el" \
	-o tag-utils.el

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	$(wget) -q -O $@                                                         \
	"https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

clean:
	$(RM) *~

distclean: clean
	$(RM) *autoloads.el *loaddefs.el TAGS *.elc
