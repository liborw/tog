# timelog: Command line time tracking made simple

## Requirements


## Summary

```
$ timelog start thesis
$ timelog stop "Finished introduction"
$ timelog status
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

**0.1**:

  - First release
  - start, stop and status commands
