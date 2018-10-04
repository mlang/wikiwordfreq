CHUNKSIZE=10M
GIT=git
STACK=stack
WGET=wget
WIKILANG=de

wikiwc.$(WIKILANG): extracted
	$(STACK) build
	$(STACK) exec wikiwc -- 'extracted/*/wiki_*' +RTS -N >$@

clean:
	rm wikiwc.$(WIKILANG) $(WIKILANG)wiki-latest-pages-articles.xml.bz2
	rm -rf extracted wikiextractor

extracted: wikiextractor
	$(PYTHON) wikiextractor/WikiExtractor.py -b$(CHUNKSIZE) -o $@ $(WIKILANG)wiki-latest-pages-articles.xml.bz2

wikiextractor:
	$(GIT) clone https://github.com/attardi/wikiextractor

$(WIKILANG)wiki-latest-pages-articles.xml.bz2:
	$(WGET) http://download.wikimedia.org/$(WIKILANG)wiki/latest/$(WIKILANG)wiki-latest-pages-articles.xml.bz2
