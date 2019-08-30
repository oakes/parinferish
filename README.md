[![Clojars Project](https://img.shields.io/clojars/v/parinferish.svg)](https://clojars.org/parinferish)

## Introduction

A Clojure and ClojureScript library that parses code and optionally applies parinfer(ish) to it. It works pretty much like parinfer and implements the three modes (indent, paren, and smart) but I'm not necessarily trying to make it behave the exact same way.

Compared to the real parinfer, parinferish...

1. Acts as a general purpose clojure parser, because its `parse` function returns the code a hiccup-style data structure, not merely a string. In fact, adjusting parens/indentation is just an optional step.
2. Doesn't ever change code when only the cursor position changes (and only smart mode requires the cursor position at all)
3. Has no basis in math, logic, or sanity...the only basis for any behavior is my own preferences and bugs.

Examples:

```
user=> (require '[parinferish.core :as ps])
nil
user=> (ps/parse "(+ 1 2)")
[[:collection [:delimiter "("] [:symbol "+"] [:whitespace " "] [:number "1"] [:whitespace " "] [:number "2"] [:delimiter ")"]]]
user=> (ps/flatten (ps/parse "(foo a" {:mode :indent}))
"(foo a)"
user=> (ps/flatten (ps/parse "(foo\na)" {:mode :paren}))
"(foo\n a)"
user=> (ps/flatten (ps/parse "([1\n 2\n 3]" {:mode :smart :cursor-line 0 :cursor-column 1}))
"([1\n  2\n  3])"
```

## Licensing

All files that originate from this project are dedicated to the public domain. I would love pull requests, and will assume that they are also dedicated to the public domain.
