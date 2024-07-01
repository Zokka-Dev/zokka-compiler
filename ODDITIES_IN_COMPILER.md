+ The current Elm compiler sends POST requests to the Elm package server for
  everything, including things that could just be GETs (e.g. requesting JSON)
+ There is a `DefineTailFunc` constructor in `AST.Optimized.Node`. This does not
  seem to be actually constructed anywhere (only destructed).
