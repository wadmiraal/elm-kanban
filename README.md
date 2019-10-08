Elm Kanban
==========

This is a Kanban tool, implemented in [Elm](). It's sole purpose is for me to learn how to code in Elm, by building a real-world app, more complex than a simple Todo MVC ;-).

Build
-----

Requires [Uglify JS](https://www.npmjs.com/package/uglify-js). Simply call:

```bash
./compile-and-optimize.sh
```

This will compile Elm, optimize it, and finally use UglifyJS' aggressive mangling to make the final file super light. See the official [Elm guide](https://guide.elm-lang.org/optimization/asset_size.html).

