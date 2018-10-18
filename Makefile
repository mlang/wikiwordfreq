CHUNKSIZE=100M
GIT=git
STACK=stack
WGET=wget
WIKILANG=de

docs/$(WIKILANG)/known.txt docs/$(WIKILANG)/known-subseq.txt: extracted.$(WIKILANG)
	$(STACK) build
	$(STACK) exec wikiwc -- -w words.$(WIKILANG) -k docs/$(WIKILANG)/known.txt -s docs/$(WIKILANG)/known-subseq.txt extracted.$(WIKILANG)/*/wiki_* +RTS -N

clean:
	rm wikiwc.$(WIKILANG) $(WIKILANG)wiki-latest-pages-articles.xml.bz2
	rm -rf extracted.* wikiextractor

extracted.$(WIKILANG): $(WIKILANG)wiki-latest-pages-articles.xml.bz2 wikiextractor
	$(PYTHON) wikiextractor/WikiExtractor.py -b$(CHUNKSIZE) -o $@ $(WIKILANG)wiki-latest-pages-articles.xml.bz2

wikiextractor:
	$(GIT) clone https://github.com/attardi/wikiextractor

$(WIKILANG)wiki-latest-pages-articles.xml.bz2:
	$(WGET) http://download.wikimedia.org/$(WIKILANG)wiki/latest/$(WIKILANG)wiki-latest-pages-articles.xml.bz2
