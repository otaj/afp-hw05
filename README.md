# MI-AFP homework #05

Homework to practice advanced work with functions and basics of typeclasses

## Task

1. You've already done some implementation of `area` and `circumference` for various shapes in the [third homework](https://github.com/MI-AFP/hw03). Now you will do something similar, but using typeclasses in `Data.Shapes`!
   * First, implement instances of  `Validable` for each shape type with function `valid` that says if given shape is valid (its attributes are acceptable, i.e., not negative, forming a triangle).
   * Then, design on your own the `Shape2D` typeclass which is a subclass of `Validable`, define `area` and `circumference` (remove dummy ones). During the design consider using `valid` - for invalid shape, both functions should return zero.
   * Finally, implement instances of `Shape2D` for each shape type.
2. Your second task is to implement *TODOs* in `Data.Logging`.
   * Write custom instances of `Show` and `Ord` for type `LogMessage` according to given test specification.
   * Implement three operators `$=` (change log level), `@@` (combine event sources), and `~~` (event source matching).
   * Use the `~~` operator and one powerful but simple higher-order function to implement `logFilter`. **Pointfree style is required!**
3. The final (and also the most interesting) task is to implement functions and instances for next data type to represent integers - the `Strinteger` (from `Data.Strinteger`)! It is a representation of an integer as English numeral string, for example, `125` is `"one hundred twenty-five"`.
   * The biggest challenge is to implement functions `integer2EngNumeral` and `engNumeral2Integer` (related with `pack` and `unpack`). Don't worry, no painful copy-pasting is waiting for you. All English numerals that are needed are already prepared in `Data.Strinteger.Helpers`. Take a look and feel free to edit as you need. Notice that `Strinteger` is an instance of `Bounded`. (Look at hints if getting stuck with those two functions.)
   * After having `pack` and `unpack` working, it will be very easy to make instances of `Eq`, `Ord`, `Num`, `Enum`, and `Integral` (*you might need some more due to dependencies*) for the `Strinteger`.
   * If you liked this task, you can try to extend it, so it also works with special named numbers like `10^100` (Googol) or `10^(10^100)` (Googolplex). Or implement functionality of `Stricimal` - string with decimal numbers (instance of `RealFloat` and related).

Hints & general requirements:

* `Shapes` and `Logging` should not be a problem. With `Strinteger` there might be a problem if you are still used to *imperative processing*. Think about elementary steps which needs to be done and what cases can occur - then put the solution together from pieces performing trivial steps. Recall how is the number composed and how do you process it when reading/writing English numeral. Always try to be **DRY**, reuse code, make simple functions and compose them.
* For joining and splitting strings by defined character, use `splitOn` and `intercalate` (similar to `words` and `unwords`). You should not hardcode any string or character in `Data.Strinteger` - use only those defined in `Data.Strinteger.Helpers` or enhance it.
* Being [DRY](https://cs.wikipedia.org/wiki/Don%27t_repeat_yourself) is essential, do not repeat code. Name expressions to reuse then, introduce helper functions.
* Local names (via `where` or `let-in`) in functions should be introduced to make the code more readable. Creating very simple helper functions in module scope is not nice. If you need some complicated functions create separate "private" module.
* Make your code as clean as possible. Prefer pattern matching and syntactic sugar everywhere it is reasonable.
* **You must understand your code completely**!

## Notes

 * In case of uncertainty, check the [dummy homework](https://github.com/MI-AFP/hw00) to recall what is the homework workflow for this course.
 * If you encounter some trouble, create an issue in your repository.
 * In case you find a bug or have an idea how to improve assignment project, create an issue or PR in this repository.
 * It might happen that tests are not 100% bulletproof, but your solution should be correct. If you want, propose tests enhancements.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE)
file for more details.
