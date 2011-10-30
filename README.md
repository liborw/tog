# Tog: The time log

Tog is simple command line time tracking utility. 

## Requirements

  - haskell-platform
  - ansi-wl-pprint

## Summary

```
$ tog start tog
starting work on tog
    at 22:47 on 30 Oct 2011

$ tog status
working on tog
    from    22:47 on 30 Oct 2011
    to now, 22:56 on 30 Oct 2011
        ==> 0.2 h have elapsed

$ tog stop
worked on tog
    from    22:47 on 30 Oct 2011
    to now, 22:59 on 30 Oct 2011
        ==> 0.2 h elapsed
```

## Data storage
The data are stored in plain-text files and can be manually edited. The storage is optimised for speed of atomic actions: start and stop.

## TODOs (in no specific order):
  - Some nice exports
  - Prettier report
  - Report of single project

## Acknowledgement
This project is inspired by command line wrapper for [basecamp][basecamp-link] you can find [here][basecamper-link]. The outputs are ispired by [timed][timed-link]. Also this is my first project in Haskell and I'm still learning.


[basecamp-link]: http://basecamphq.com/
[basecamper-link]: https://github.com/klondike/basecamper
[timed-link]: http://adeel.github.com/timed/

## Changelog

**0.5**

  - Better report
  - Pretty output of start, stop and status (inspired by [timed][timed-link])

**0.4**:

  - Bash completion
  - Nicer report
  - Edit command for quick peek to storage files
  - Help and usage info

**0.3**:

  - Renamed to Tog: The time log
  - Use cabal build system

**0.2**:

  - Log past work
  - Report of overall activity

**0.1**:

  - First release
  - start, stop and status commands
