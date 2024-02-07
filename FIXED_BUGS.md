# List of bugs 

These are a list of known bugs in version 0.19.1 of the Elm compiler which
Zokka fixes.

+ String literals are not sanitized when generating HTML files:
  [https://github.com/elm-lang/elm-make/issues/174](https://github.com/elm-lang/elm-make/issues/174)
  linked from
  [https://github.com/elm/compiler/issues/1377](https://github.com/elm/compiler/issues/1377)
+ Improper tail-call optimization leading to inconsistent, sometimes crashing
  code when using closures (this actually was two separate bugs which are both
  fixed):
    - [https://github.com/elm/compiler/issues/2017](https://github.com/elm/compiler/issues/2017)
    - [https://github.com/elm/compiler/issues/1813](https://github.com/elm/compiler/issues/1813)
    - [https://github.com/elm/compiler/issues/2268](https://github.com/elm/compiler/issues/2268)
    - [https://github.com/elm/compiler/issues/2221](https://github.com/elm/compiler/issues/2221)

## Improper TCO

The problem with Elm 0.19.1's implementation of TCO was that it would overwrite
local variables and arguments in a while loop that could be exposed to the
outside world via a closure.

Here are two examples that demonstrate different issues.

For example 1, this Elm code

```elm
tcoMakeLazy : List a -> List (() -> a) -> List (() -> a)
tcoMakeLazy list accum =
    case list of
        itemEscape :: items ->
            tcoMakeLazy items ((\_ -> itemEscape) :: accum)

        _ ->
            accum
```

would compile in the mainline Elm 0.19.1 compiler to

```javascript
var $author$project$TCOProducesBadClosures$tcoMakeLazy = F2(
	function (list, accum) {
		tcoMakeLazy:
		while (true) {
			if (list.b) {
				var itemEscape = list.a;
				var items = list.b;
				var $temp$list = items,
					$temp$accum = A2(
					$elm$core$List$cons,
					function (_v1) {
						return itemEscape;
					},
					accum);
				list = $temp$list;
				accum = $temp$accum;
				continue tcoMakeLazy;
			} else {
				return accum;
			}
		}
	});
```

which exposes the fact that `itemEscape` is overwritten by TCO to the outside
world via the closure in the anonymous function.

In Zokka we instead compile to

```javascript
var $author$project$TCOProducesBadClosures$tcoMakeLazy = F2(
	function ($tailcallfunctionparam$list, $tailcallfunctionparam$accum) {
		var $sentinel$tcoMakeLazy = {};
		var $tailcallloophoist$tcoMakeLazy = function () {
			var list = $tailcallfunctionparam$list,
				accum = $tailcallfunctionparam$accum;
			if (list.b) {
				var itemEscape = list.a;
				var items = list.b;
				var $temp$list = items,
					$temp$accum = A2(
					$elm$core$List$cons,
					function (_v1) {
						return itemEscape;
					},
					accum);
				$tailcallfunctionparam$list = $temp$list;
				$tailcallfunctionparam$accum = $temp$accum;
				return $sentinel$tcoMakeLazy;
			} else {
				return accum;
			}
		};
		tcoMakeLazy:
		while (true) {
			var $tailcallloopreturn$tcoMakeLazy = $tailcallloophoist$tcoMakeLazy();
			if ($tailcallloopreturn$tcoMakeLazy === $sentinel$tcoMakeLazy) {
				continue tcoMakeLazy;
			} else {
				return $tailcallloopreturn$tcoMakeLazy;
			}
		}
	});
```

For example 2, the Elm code

```elm
g : Int -> (Int -> a) -> a
g value cont =
    case value of
        1 -> cont 1
        _ -> g (value-1) (\result -> cont (result * value))

```

would compile to this JS in Elm 0.19.1

```javascript
var $author$project$TCOMiscompilation0$g = F2(
	function (value, cont) {
		g:
		while (true) {
			if (value === 1) {
				return cont(1);
			} else {
				var $temp$value = value - 1,
					$temp$cont = function (result) {
					return cont(result * value);
				};
				value = $temp$value;
				cont = $temp$cont;
				continue g;
			}
		}
	});
```

which exposes the TCO implementation detail that `value` is getting overwritten
on every loop to the outside world via the closure created by the anonymous
function.

Now instead we compile to

```javascript
var $author$project$TCOMiscompilation0$g = F2(
	function ($tailcallfunctionparam$value, $tailcallfunctionparam$cont) {
		var $sentinel$g = {};
		var $tailcallloophoist$g = function () {
			var value = $tailcallfunctionparam$value,
				cont = $tailcallfunctionparam$cont;
			if (value === 1) {
				return cont(1);
			} else {
				var $temp$value = value - 1,
					$temp$cont = function (result) {
					return cont(result * value);
				};
				$tailcallfunctionparam$value = $temp$value;
				$tailcallfunctionparam$cont = $temp$cont;
				return $sentinel$g;
			}
		};
		g:
		while (true) {
			var $tailcallloopreturn$g = $tailcallloophoist$g();
			if ($tailcallloopreturn$g === $sentinel$g) {
				continue g;
			} else {
				return $tailcallloopreturn$g;
			}
		}
	});
```


Each of these examples motivate each of the two changes that Zokka makes to TCO.

The first change to address the issues with the first example that Zokka makes
here is to hoist out the body of the `while` loop into its own function prefixed
by `$tailcallloophoist$`. This lets us get around scoping issues associated with
`var` that we see with the second example.  We could fix this by using something
like a `let`, but this would involve a change to what version of Javascript Elm
compiles to. In particular Elm currently compiles to ES5 (and almost ES3 as per
[https://discourse.elm-lang.org/t/about-ecmascript-supported-by-elm/9036](https://discourse.elm-lang.org/t/about-ecmascript-supported-by-elm/9036)),
but `let` is only available in ES6. This does unfortunately involve the
performance hit of a function call where no function call existed previously, as
current JS engines (January 2024) don't seem to be able to optimize away the
function call we're introducing.

The second change to deal with the second example that Zokka makes here is to
introduce `$tailcallfunctionparam$`s. These parameters are copied in the hoisted
while loop body and those copies are then used in any closures, while the
`$tailcallfunctionparam$` are overwritten. This isolates any values that go out
through closures from the overwriting of function parameters that Elm's TCO
does.
