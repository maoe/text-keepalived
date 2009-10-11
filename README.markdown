haskell-keepalived
==================

haskell-keepalived is a parser library and a command-line tool for keepalived.conf.


REQUIREMENTS
------------------
haskell-keepalived package requires following packages:

- [parsec](http://hackage.haskell.org/package/parsec) >= 3
- [filepath](http://hackage.haskell.org/package/filepath)
- [FileManip](http://hackage.haskell.org/package/FileManip)
- [syb](http://hackage.haskell.org/package/syb)
- [cmdargs](http://hackage.haskell.org/package/cmdargs)

All of these are available from [Hackage][1].


INSTALLATION
------------------
You can configure, build, and install all inthe usual way with Cabal:

    runhaskell Setup.hs configure
    runhaskell Setup.hs build
    runhaskell Setup.hs install


USAGE
------------------
    kc verify [FLAG] [FILE]
      Verify configuration files.
    
    kc dump [FLAG] [FILE]
      Dump configuration files.
    
    Common flags:
      -? --help[=FORMAT]  Show usage information (optional format)
      -V --version        Show version information
      -v --verbose        Higher verbosity
      -q --quiet          Lower verbosity



[1]: http://hackage.haskell.org/
