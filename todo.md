* proper errors
* default arguments in std
* captures
* lambdas
* piping
* generic specialization
* intern zero and one
* limit resolution default? we can probably make an infinite loop with this
* cross-recursive types
* function equality/hashing
* method with specializations (a.cast<...>)
* lazy parameters in ud?
* intern unknown type
* intern tail type
* intern tail object
* handle 1-tuple types
* have an option for global runtimes, reduce size of allocated objects
    * alternatively, have an interned list of runtimes, and store the id in the object
* shortcut variant constructors with a nill value
* mapping().set(...) results in an unknown eq call, an easy way to prevent this is to disallow "free" local generic variables at resolve time
* automatic disambiguation? see script 73, can we delete the disambiguation?
* better tuple equality?
* the `pub`s should be reduced