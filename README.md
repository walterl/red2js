# walterl/red2js

Convert a [Node-RED](https://nodered.org/) flow data to JavaScript pseudo code.

**Generated code is NOT executable!** red2js output is intended to aid in
understanding complex Node-RED flows, not to generate drop-in replacements for
Node-RED.

## Installation

Download from https://github.com/walterl/red2js

## Usage

Specify the Node-RED `flows.json` file to red2js, and get the JavaScript pseudo code on stdout:

    $ clj -M:run-m flows.json

Run the project's CI pipeline and build an uberjar:

    $ clj -T:build ci

Run that uberjar:

    $ java -jar target/red2js-0.1.0-SNAPSHOT.jar

## License

Copyright © 2022 Walter

Distributed under the [Eclipse Public License version 1.0](./LICENSE).
