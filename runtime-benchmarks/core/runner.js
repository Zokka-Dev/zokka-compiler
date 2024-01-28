const { Elm } = require("./elm.js");

const app = Elm.V8.Benchmark.init();
let results = [];
app.ports.reportResults.subscribe(function(message) {
  results = message;
  console.log(JSON.stringify(results, null, 2));
  process.exit(0);
});

