Hack to turn the profiling output into the format suitable for FlameGraph.

[![Build Status](https://travis-ci.org/llelf/flamingra.svg?branch=master)](https://travis-ci.org/llelf/flamingra)

# Haddock copy:

(Click it, it's “interactive”.)

![Flame graph](http://lelf.lu/files/pandoc.svg)

# TL;DR (and the only one available) guide:

```sh
some-program +RTS -P
wget https://github.com/brendangregg/FlameGraph/raw/master/flamegraph.pl
chmod +x flamegraph.pl
flamingra some-program.prof | ./flamegraph.pl picturesque.svg
```

# Usage:

```
flamingra out.prof | …
```
or
```
… | flamingra | …
```
where out.prof is @-P@ profiling output.

