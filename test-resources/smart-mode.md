# Smart Mode

## Leading Close-Parens

Leading close-parens can cause many problems that can be fixed by paren mode,
so we exit to paren mode when they are detected.

For example, it is convenient to keep trailing parens in front of the cursor
after pressing enter or after deleting everything behind them:

```in
(let [a 1
      |])
```

```out
(let [a 1
      |])
```

Moving the cursor away:

```in-disable
(let [a 1
      ]); <-- spaces
```

```out-disable
(let [a 1])
      ; <-- spaces
```

**NOTE: Parinferish behaves differently.** It does not change any code when the cursor moves:

But we also need safety from inadvertent AST breakage.  For example,
Indent Mode should allow this intermediate state:

```in
(let [a 1
      |] (+ a 2))
```

```out
(let [a 1
      |] (+ a 2))
```

Moving the cursor away will cause Indent Mode to still detect the leading
close-paren, exit to Paren Mode, then fix the spacing to prevent inadvertent
breakage.

```in-disable
(let [a 1
      ] (+ a 2))
```

```out-disable
(let [a 1]
     (+ a 2))
```

**NOTE: Parinferish behaves differently.** It does not change any code when the cursor moves:

To prevent weird things, indentation needs to be locked to respect
the leading close-paren.  Exiting to Paren Mode allows this and prevents further
AST breakage.

```in
(let [a 1
  |] (+ a 2))
```

```out-disable
(let [a 1
      |] (+ a 2))
```

**NOTE: Parinferish behaves differently.** It will not allow the end bracket to be there:

```out
(let [a 1]
   |(+ a 2))
```

Moving cursor to the right progressively moves leading close-parens behind it
to their normal positions:

```in
(let [a 1
      ]|)
```

```out-disable
(let [a 1]
     |)
```

**NOTE: Parinferish behaves differently.** It does not move the end bracket:

```out
(let [a 1
      ]|)
```

When in Paren Mode we must abide by its rules to stay balanced.

As a courtesy, unmatched close-parens in a paren trail at the beginning of a
line are auto-removed (only when paren mode is triggered from smart mode).

```in
|)
```

```out
|
```

```in
(foo
  |))
```

```out
(foo
  |)
```

```in
(foo
  }|)
```

```out
(foo
  |)
```

Likewise:

```in
(foo
  ) foo} bar|
```

```out-disable
(foo
  ) foo} bar|
       ^ error: unmatched-close-paren
```

**NOTE: Parinferish behaves differently.** It removes the invalid end bracket:

```out
(foo
  ) foo bar|
```

```in
(foo
  ) (bar|
```

```out-disable
(foo
  ) (bar|
    ^ error: unclosed-paren
```

**NOTE: Parinferish behaves differently.** It adds the necessary paren:

```out
(foo
  ) (bar|)
```


## Changes

Indent a single-line expression to enter a sibling:

```in
(foo (bar)
      |baz)
```

```out
(foo (bar
      |baz))
```

Dedent multi-line expression to leave its parent:

```in
(foo
|{:a 1
   :b 2})
```

```out-disable
(foo)
{:a 1
 :b 2}
```

**NOTE: Parinferish behaves differently.** It does not maintain relative indentation:

```out
(foo)
{:a 1
   :b 2}
```

Indent multi-line expression to enter new parent:

```in
(foo)
  |{:a 1
 :b 2}
```

```out
(foo
  {:a 1
   :b 2})
```

Dedenting an inner line makes it leave parent:

```in
(foo
  {:a 1
|:b 2})
```

```out
(foo
  {:a 1})
:b 2
```

Dedenting a collection will adopt a former sibling line below it:

```in
(defn foo
|[a b]
  bar)
```

```out-disable
(defn foo)
[a b
  bar]
```

**NOTE: Parinferish behaves differently.** It doesn't perform indent mode on collections starting after the cursor:

```out
(defn foo)
[a b]
  bar
```

But dedenting a top-level form should not cause a child to adopt a sibling:

```in
|(defn foo
    [a b]
    bar)
```

```out-disable
(defn foo
  [a b]
  bar)
```

**NOTE: Parinferish behaves differently.** It does not maintain relative indentation:

```out
(defn foo
    [a b]
    bar)
```

Indented comments move with expressions:

```in
|(defn foo
    [a b]
    ; comment 1
    bar)
    ; comment 2
```

```out-disable
(defn foo
  [a b]
  ; comment 1
  bar)
  ; comment 2
```

**NOTE: Parinferish behaves differently.** It does not maintain relative indentation:

```out
(defn foo
    [a b]
    ; comment 1
    bar)
    ; comment 2
```

## Cursor temporarily preventing sibling adoption

To prevent undesirable sibling adoption when dedenting, we temporarily keep
a close-paren from moving when the cursor is to the left of its open-paren.

```in
(defn foo
|[a b
   c d]
  bar
  baz)
```

```out-disable
(defn foo)
|[a b
 c d]
  bar
  baz
```

**NOTE: Parinferish behaves differently.** It does not maintain relative indentation:

```out
(defn foo)
|[a b
   c d]
  bar
  baz
```

```in
(defn foo)
|[a b
 c d]
  bar
  baz
```

```out
(defn foo)
|[a b
 c d]
  bar
  baz
```

## Multiple Changes

```in-disable
(my-fnfoo (if some-condition
 -----+++
         println) my-funfoo {:foo 1
                  ------+++
                          :bar 2})
```

```out-disable
(foo (if some-condition
       println) foo {:foo 1
                     :bar 2})
```

## Resolving Precarious Paren After Dedent

In the example below, we expect `4` to not be adopted
by any collection inside `(((1 2 3)))`.

```in
(|(((1
        2
        3)))
    4)
```

```out-disable
(|(((1
    2
    3)))
    4)
```

**NOTE: Parinferish behaves differently.** It does not maintain relative indentation:

```out
(|(((1
        2
        3)))
    4)
```

When cursor is removed, the precarious parens are resolved by preserving structure
and correcting indentation.

```in
(|(((1
    2
    3)))
    4)
```

```out-disable
((((1
    2
    3)))
 4)
```

**NOTE: Parinferish behaves differently.** It doesn't perform paren mode on collections starting before the cursor:

```out
((((1
    2
    3)))
    4)
```

```in
((|((1
    2
    3)))
    4)
```

```out-disable
((|((1
    2
    3)))
 4)
```

**NOTE: Parinferish behaves differently.** It doesn't perform paren mode on collections starting before the cursor:

```out
((|((1
    2
    3))
    4))
```

## Indenting Selected Lines

Indent only the first line:

```in
  |(foo
  (bar
    baz))
```

```out-disable
  (foo
    (bar
      baz))
```

**NOTE: Parinferish behaves differently.** It only corrects indentation with the minimum number of spaces:

```out
  (foo
   (bar
    baz))
```

**NOTE: The rest of the tests below are currently disabled for Parinferish.**

Indent first two lines:

```in-disable
  (foo
++
    (bar
++
    baz))
```

```out-disable
  (foo
    (bar
      baz))
```

Indent last two lines:

```in-disable
  (foo
      (bar
++
        baz))
++
```

```out-disable
  (foo
      (bar
        baz))
```


Indent only the first line:

```in-disable
  |(foo
  bar
  baz)
```

```out-disable
  (foo
    bar
    baz)
```

Indent first two lines:

```in-disable
  (foo
++
    bar
++
  baz)
```

```out-disable
  (foo
    bar
    baz)
```

Indent last two lines:

```in-disable
(foo
    bar
++
    baz)
++
```

```out-disable
(foo
    bar
    baz)
```

## Multi-change Bug

[Issue #173](https://github.com/shaunlebron/parinfer/issues/173)

```in-disable
((reduce-kv (fn [m k v]
+
            {}
           +
            {}))
           +
```

```in-disable
((reduce-kv (fn [m k v]
            {}
            {})))
                +
```

```out-disable
((reduce-kv (fn [m k v])
            {}
            {}))
```

[Issue #176](https://github.com/shaunlebron/parinfer/issues/176)

```in-disable
(let [a 1]
  (
  +
    (foo))
  ++
```

```in-disable
(let [a 1]
  (
    (foo)))
         +
```

```out-disable
(let [a 1]
  (
    (foo)))
```

[Issue #177](https://github.com/shaunlebron/parinfer/issues/177)

```in-disable
(let [a 1]

  (foo))
```

```in-disable
(let [a 1]
  (let [a 1]
  +++++++++++
  (foo))
++++++++
  (foo))
```

```in-disable
(let [a 1]
  (let [a 1]
    (foo))
  ++
  (foo))
```

```out-disable
(let [a 1]
  (let [a 1]
    (foo))
  (foo))
```

[Issue #179](https://github.com/shaunlebron/parinfer/issues/179)

```in-disable
{:a                 {:b              (Integer/valueOf (-> ""
    ----------------
                                                          (.length)))}}
```

```in-disable
{:a {:b              (Integer/valueOf (-> ""
        -------------
                                                          (.length)))}}
                             -----------------------------
```

```out-disable
{:a {:b (Integer/valueOf (-> ""
                             (.length)))}}
```
