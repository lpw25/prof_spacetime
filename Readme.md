# Prof_spacetime

`prof_spacetime` is a viewer for OCaml "spacetime" profiles.

## Installation

The easiest way to install is using OPAM:

    opam install prof_spacetime

For installing from source see the dependencies and build instructions
in the `opam` file.

## Creating a spacetime profile

To create a spacetime profile from a run of an executable you must build
that executable with a spacetime-enabled OCaml compiler:

    opam switch 4.04.0+spacetime
    eval `opam config env`
    make foo.exe

Then run your executable with the `OCAML_SPACETIME_INTERVAL` environment
variable set to an interval in milliseconds between snapshots:

    OCAML_SPACETIME_INTERVAL=100 foo.exe test-data

this will produce a file with a name like `spacetime-1234` where `1234`
was the pid of the process.

## Viewing profiles in the browser

To view a spacetime profile in the browser use the `serve` command:

    prof_spacetime serve spacetime-1234

By default, this will serve the profile on `127.0.0.1:8080`. The address
and port can be changed with the `--address` and `--port` options.

## Viewing profiles in the terminal

To view a spacetime profile in the terminal use the `view` command:

    prof_spacetime view spacetime-1234

## Preprocessing spacetime profiles

Processing a spacetime profile can take a while. To avoid redoing this
work each time you want to serve the profile you can use the `process`
command:

    prof_spacetime process spacetime-1234

which will create a preprocessed version of the profile
`spacetime-1234.p`. This can then be passed to the `serve` or `view`
commands using the `-p` flag:

    prof_spacetime serve -p spacetime-1234.p

## Locations for C function calls

To get locations for C function calls, you should also use the `-e`
option to pass `prof_spacetime` the executable from which the profile was
generated. This works with all commands:

    prof_spacetime serve -e foo.exe spacetime-1234
