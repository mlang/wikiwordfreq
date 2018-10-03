# wikiwc

A Wikipedia word frequency counter.

This project makes use of [Wikipedia_Extractor](http://medialab.di.unipi.it/wiki/Wikipedia_Extractor)
to pre-process a full Mediawiki dump into basically plain text files.
It then parses these files into separate words, and counts the number
of occurences of each word.

## Usage

As a default, wikiwc downloads the german wikipedia.

```shell
$ make WIKILANG=en
```

