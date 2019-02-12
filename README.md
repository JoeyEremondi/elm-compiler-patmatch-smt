# The Elm Compiler

Learn about the Elm programming language at [elm-lang.org](http://elm-lang.org/).

This is a fork of the Elm Compiler maintained by CurrySoftware GmbH.
We merged a few bug fixes by the community and also expanded the reactor.

## Build

With Stack:

1. Install Stack (https://www.haskellstack.org)
2. Clone the repo
3. `cd compiler`
4. `stack init`
5. `stack build`


## Install

The built Elm executable will reside inside `.stack-work/dist/<your-arch>/<cabal-version>/build/elm/elm`.
To install it link or copy it to the `/usr/bin/` folder.

For example:

`copy .stack-work/dist/x86_64-linux-tinfo6/Cabal-2.0.1.0/build/elm/elm /usr/bin/elm-dev`

or
`ln -s .stack-work/dist/x86_64-linux-tinfo6/Cabal-2.0.1.0/build/elm/elm /usr/bin/elm-dev`


## Using the new Features in Elm Reactor

This Elm-fork contains two new features in the Elm reactor:

1. The reactor can now emit compiled JavaScript instead of Html with the `output=js` parameter:

```html
<script src="http://localhost:8000/src/Main.elm?output=js" charset="utf-8"></script>
```

2. The reactor can now emit code compiled in debug mode with the `debug=true` parameter:

`http://localhost:8000/src/Main.elm?debug=true`

or in combination with JavaScript output:
```html
<script src="http://localhost:8000/src/Main.elm?output=js&debug=true" charset="utf-8"></script>
```


For a working example check out [elm-reactor-example](https://github.com/CurrySoftware/elm-reactor-example).


## Future

We are currently exploring the idea of building a private Elm package repository to facilitate usage of Elm in corporate environments.

Currently, it is not possible to use either local packages or another package repository than `package.elm-lang.org`.

We want to change that!

If you are a corporate Elm-user and want to support us or if you are interested in a closed beta please contact us through [elm@curry-software.com](mailto:elm@curry-software.com).

## Support

Please contact [elm@curry-software.com](mailto:elm@curry-software.com) for support.
