# Stringlines

Stringlines is a tool for polling real-time transit data and
generating 2D space-time charts (sometimes referred to as "stringline
charts" or "Marey diagrams" after [their
creator](https://en.wikipedia.org/wiki/%C3%89tienne-Jules_Marey)) of
the resulting data. Currently, the only supported source of data on
the backend is the MBTA's
[API](https://www.mbta.com/developers/v3-api). Data is polled and
stored in a simple SQLite databse. A second program queries this data
and uses Gnuplot to produce the final output.

## Usage

### Polling

Parameters for polling are currently stored in the source in
`app/Main.hs`. These will be moved to command line arguments soon. To
run the polling executable, just do `stack run bus-scrape-poll`.

### Chart generation

This step can be run with `stack run bus-scrape-make-chart` followed
by a `--` and the rest of the command line arguments. These are mostly
self-documented in the usage message - note that you'll need to
specify `-r` for a route ID and `-d` for a day.

## TODO

A whole bunch of things. Here are some of the more important items:

- Support routes with an asymmetry in the two directions, probably by
  allowing separate charts to be generated for different directions.
  Should still allow symmetric routes to be plotted on the same chart.
- Visual improvements to the output.
- Command line arguments for the polling executable. Routes will
  probably need to be configured via a config file.
- Make sure modules only export what they need to export.
- Refactor the data gathering to at least make using other data
  sources possible.
- Update time zone handling to store the original zoned time returned
  by the API instead of just UTC. Will require adding `FromField` /
  `ToField` instances for the Haskell `ZonedTime` type.
- Look into whether I actually need Jord or if I can use a more
  lightweight library that's supported by default in the solver I'm
  using.
