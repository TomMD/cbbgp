# CBBGP: Clone-Build-Bench-Graph-Push

CBBGP is a LINUX ONLY script for pulling from one or more repositories,
building, benchmarking, producing performance graphs, and pushing these results
(SVG graphs included) to a repository with a hard-coded directory structure.

# Use

Just call `cbbgp [-p seconds] cfg.ini`.

```
Parameters:
  Configuration    An INI configuration file

Flags:
  -p seconds  --period=seconds  Number of seconds between each run (default: 1 day)
```

CBBGP is configured by an INI file (yes, old school is back!) which is in the form:

```
[ProjectName]
repo=<git repository URL>
builder=<shell command to build the code, for example `make`, ./install.sh, or stack install>
resultRepo=<git repository URL for the result>
freeform_benchmark_name1=<command to be benchmarked>
freeform_benchmark_name2=<command to be benchmarked>
freeform_benchmark_name3=<command to be benchmarked>

[Project-2]
...
[Project-N]
```

The specified result repository will be updated with files in `$project/` of:

 * `$date.raw.results`: Raw text results (in the form `show :: HashMap Text (Double,Double)`)
 * `$benchmark.{memory,time}.svg`: Graphs of all `*.results` for the named benchmark.
