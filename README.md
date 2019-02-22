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

Parameters for the polling script live in a `config.yaml` file. As an
example, one might have:

```yaml
api_key: "***REMOVED***"
routes:
  - id: "1"
    shape_ids:
      - "010058"
      - "010070"
```
	  
Each route needs zero, one, or two shape IDs to use for comparing the
present position of vehicles against for the purpose of calculating
progress. If no shape IDs are specified, no calculation is done. If
two are specified, the shape to use will be chosen intelligently based
on which one matches the direction the vehicle is traveling. To run
the polling executable, just do `stack run stringlines-poll`.

### Chart generation

This step can be run with `stack run stringlines-chart` followed by a
`--` and the rest of the command line arguments. These are mostly
self-documented in the usage message - note that you'll need to
specify `-r` for a route ID and `-d` for a day. Additionally, you can
choose `--direction-0` or `--direction-1` to plot vehicles traveling
in only one direction - this is most useful for when differences
between the routing in the inbound and outbound directions require
using two shapes and the progress in the two directions is therefore
not directly comparable.

## TODO

A whole bunch of things. Here are some of the more important items:

- Visual improvements to the output.
- Refactor the data gathering to at least make using other data
  sources possible.
- Update time zone handling to store the original zoned time returned
  by the API instead of just UTC. Will require adding `FromField` /
  `ToField` instances for the Haskell `ZonedTime` type.
- Look into whether I actually need Jord or if I can use a more
  lightweight library that's supported by default in the solver I'm
  using.
- Do calculation of progress at chart generation rather than polling.
  Refactor calculation of progress to not parse the polyline for every
  data point. On the other hand, if I do a web front-end, I'll want to
  keep the approach of calculating progress as I poll.
- Support charting shared trunks where several routes converge.
- Use `fieldLabelModifier` to clean up a bunch of duplicate record
  fields in the API and probably clean up some code as a result.
