-- |
-- It will be (kind of) interactive (e.g. <http://lelf.lu/files/pandoc.svg>)
--                      .
-- <<pandoc.svg>>
--  
-- = TL;DR (and the only one available) guide:
-- 
-- > some-program +RTS -P
-- > wget https://github.com/brendangregg/FlameGraph/raw/master/flamegraph.pl
-- > chmod +x flamegraph.pl
-- > flamingra some-program.prof | ./flamegraph.pl > picturesque.svg
-- 
-- = Usage:
-- 
-- > flamingra out.prof | …
-- 
-- or
-- 
-- > … | flamingra | …
-- 
-- where out.prof is @-P@ profiling output.

module Flamingra.Docs where

