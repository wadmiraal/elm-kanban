Elm Kanban [![Build Status](https://travis-ci.org/wadmiraal/elm-kanban.svg?branch=master)](https://travis-ci.org/wadmiraal/elm-kanban)
==========

This is a Kanban tool, implemented in [Elm](https://elm-lang.org/). It's sole purpose is for me to learn how to code in Elm, by building a real-world app that is more complex than a simple Todo app.

You can see it in action [here](https://wadmiraal.github.io/elm-kanban/index.html).

Build
-----

Simply call:

```bash
./build.sh
```

This will compile Elm, optimize it, and finally use UglifyJS' aggressive mangling to make the final file super light. See the official [Elm guide](https://guide.elm-lang.org/optimization/asset_size.html) for more information on optimizing Elm assets.

