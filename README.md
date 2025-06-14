[![CircleCI](https://circleci.com/gh/arenadotio/pgx.svg?style=shield)](https://circleci.com/gh/arenadotio/pgx)
[![Coverage Status](https://coveralls.io/repos/github/arenadotio/pgx/badge.svg?branch=master)](https://coveralls.io/github/arenadotio/pgx?branch=master)
[![Documentation](https://img.shields.io/badge/documentation-odoc-blue)](https://arenadotio.github.io/pgx/index.html)

PGX is a pure-OCaml PostgreSQL client library, supporting EIO, LWT, Async, or
synchronous operations.

This library focuses on correctness and safety, with features like:

 - It is nearly impossible to try to execute a prepared statement that hasn't
   been prepared.
 - Trying to run multiple queries at the same time will work properly (although
   there's no performance benefit, since we currently don't send queries in
   parallel).
 - Lots of automated tests.
 - `Pgx.Value` for parameters and returned data, encouraging people to use
   the built-in converters instead of trying to handle everything as a string.
 - EIO, LWT and Async support are built in, no need to write your own IO module.
 - Mirage OS is supported via Pgx_lwt_mirage

We also provide a relatively high-level interface, like `Pgx_async.execute_pipe`,
which prepares a statement, executes it with the given parameters, returns an
`Async.Pipe.Reader.t` (so you can stream results), and unprepares the statement
when the query is finished.

Significant portions of the code come from [PG'Ocaml](http://pgocaml.forge.ocamlcore.org/).

This community project was originally developed at `arenadotio/pgx` from
where it was forked [as recommended by the former maintainer](https://github.com/arenadotio/pgx/pull/134#issuecomment-2651239638).

## Setup

```
opam install pgx_eio # or pgx_lwt_unix or pgx_async or pgx_unix or pgx_lwt_mirage
```

## Examples

See [pgx_async/bin/pgx_async_example.ml](pgx_async/bin/pgx_async_example.ml) for
a complete example of the high-level functional interface. To translate the
example to Lwt, replace `Pgx_async` with `Pgx_lwt` and `>>|` with `>|=`. To
translate it to EIO, use `Pgx_eio` and replace both `>>|` and `>>=`
with `|>`, or just replace `>>| fun () ->` with `;`.

I.e. in `Pgx_eio`, you can replace:

```ocaml
Pgx_async.execute ~params "INSERT INTO ..."
>>| fun () ->
```

... with:

```ocaml
Pgx_eio.execute ~params "INSERT INTO ...";
```
