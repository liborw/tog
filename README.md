# Tog: The time log

Tog is simple command line time tracking utility. 

## Requirements


## Summary

```
$ tog start thesis
$ tog stop "Finished introduction"
$ tog status
```

## Data storage
The data are stored in plain-text files and can be manually edited. The storage is optimised for speed of atomic actions: start and stop.

## TODOs (in no specific order):
  - Bash completion
  - To be able to log activity
  - Pretty printing 
  - Some nice exports
  - It would be nice to connect it to git-flow
  - External commands (just to try)

## Acknowledgement
This project is inspired by command line wrapper for [basecamp][basecamp-link] you can find [here][basecamper-link]. Also this is my first project in Haskell and is still in development.

[basecamp-link]: http://basecamphq.com/
[basecamper-link]: https://github.com/klondike/basecamper

## Changelog

**0.3**:

  - Renamed to Tog: The time log
  - Use cabal build system

**0.2**:

  - Log past work
  - Report of overall activity

**0.1**:

  - First release
  - start, stop and status commands
