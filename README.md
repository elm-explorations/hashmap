**WARNING:** This is an experiment. Try it out, give feedback, but it should not be used in production. It may fail as an experiment independent of your scenario.

<br>

# Hashmap Experiment

This package implements a hashmap in Elm. Rather than comparing keys like in `Dict`, keys are hashed to an `Int` first.

If you use this, the goal should be to get benchmark data from real-world scenarios to share with us here.

<br>


## Challenges

Like with `comparable`, not everything is `hashable`. To be able to experiment without pre-committing to language changes, the experiment cannot rule out the use of unhashable values, like functions and JS objects.

Implementing `Dict.fold` is also weird. Since we are hashing keys, the order of the elements will be seemingly random. So it is not really clear what "folding from the right" means in that context. Having a function that reveals the specifics of the hashing function definitely a major weakness in this API because it means that folks may rely on the specific ordering and make it impossible to ever change the hashing function.

<br>

## Trying It Out

The package has almost the same API as `Dict`, so you can change `import Dict` to `import Hash.Dict as Dict` to give it a try.

Remember to change `Dict.foldl` and `Dict.foldr` to `Dict.fold`. Please let us know in issues if this leads to scenarios that do not work anymore. Do you actually need them to be ordered for some reason? What is the reason?

<br>

## License

This library uses the BSD3 License. See LICENSE for more information.
