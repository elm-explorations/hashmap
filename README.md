## Elm Hashmap Exploration

This library implements a proper hashmap for Elm. Keys are hashed instead of compared for ordering, so that any type can be used as keys.

Keep in mind that this is an experiment. Try it out, give feedback, but keep in mind that it could go away at some point.

The library almost maintains API compatability with `Dict`. To make use of this library, change the import statement from:

`import Dict`

to:

`import Hash.Dict as Dict`

And change all instances of `Dict.foldl` or `Dict.foldr` into `Dict.fold`. Since we are hashing and not comparing, the order of the elements will be seemingly random, and so it won't matter what directing you're folding in.

## License

This library uses the BSD3 License. See LICENSE for more information.
