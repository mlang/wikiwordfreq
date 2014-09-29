Count word frequencies of wikipedia dumps
=========================================

This program takes a wikipedia XML dump from STDIN and produces word-frequency
counts to STDOUT.  It was originally developed to acquire a large
list of words for a particular language, to generate a verification
table for BRLTTY braille contraction tables.  However, it can
be used for all sorts of things.

Example usage (streaming)
-------------------------

```bash
cmake .
make
wget -qO- http://dumps.wikimedia.org/dewiki/latest/dewiki-latest-pages-articles.xml.bz2 |
bzcat | ./dewiki-words
```

If you are experimenting with this tool, make sure to fetch the XML dump
(it is rather big, currently about 3.5GB) once to minimize bandwidth consumption
on dumps.wikimedia.org.

Known Problems
--------------

It looks like some articles are crashing the GCC 4.9 regex engine.
With the current latest dump, dewiki-words terminates with a segmentation fault
shortly after the 1100000th page.  I have yet to isolate the case.

