<img src="https://github.com/J0HNN7G/LogiLan/blob/master/doc/Icon.png" width="155" height="110">

# LogiLan
- A toy functional language to show the Curry-Howard isomorphism.
- The function `prove` takes any logiLan function as input and will rewrite the function's type expression as intuitionistic propositional logic (IPL). Finally, the corresponding proof tree is shown in the command-line. 
- `prove` shows a direct link between computer programs (functional programming) and mathematical proofs (intuitionistic logic), i.e., the Curry-Howard isomorphism.
- Given that `prove` may be used on falsifiable functions, the compiler will ignore the given function if it is a parameter for `prove`. However, if a falsifiable (and thereby erroneous) function is applied anywhere else, you will get a compile-time error. 

### Documentation

### Acknowledgements

- The creation of this language is would not have been possible without [Stephen Diel's](https://www.stephendiehl.com/) tutorial, [Write You a Haskell](http://dev.stephendiehl.com/fun/).
- Thank you [Philip Wadler](http://homepages.inf.ed.ac.uk/wadler/) and [Michael Fourman](https://www.inf.ed.ac.uk/people/staff/Michael_Fourman.html) for teaching me about functional programming and computational logic.
