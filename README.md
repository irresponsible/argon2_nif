The irresponsible software guild presents...

# argon2_erl

The argon2 secure password hashing function, now usable from your rebar3 project

## Usage:

Add this repo to your rebar.config `deps`:

```
{argon2, {git, "https://github.com/irresponsible/argon2_nif", {branch, master}}}
```

And this to your `.app.src`'s `applications`: `argon2`. This will load the `argon2` and `argon2_nif` modules. The public API (module `argon2`) is really simple:

```erlang
1> argon2:verify(<<"hello">>, argon2:hash(<<"hello">>)).
true
```

There are also 2- and 3-arity versions of the `argon2:hash` function which provide for using a provided salt and different security options.

## Requirements

An erlang with dirty nifs enabled is required. This is default from R20 onwards but must be enabled during build for previous erlangs. Dirty nifs run on a separate OS thread and because argon2 is an expensive function (ideally it should take several seconds to hash whereas blocking for more than a millisecond is a bad idea ), this helps to avoid messing up the erlang scheduler.

Building also requires a c compiler, GNU Make and rebar3

## Building

Build with:

```shell
rebar3 compile
```

Run tests with:

```shell
rebar3 eunit  # unit tests
rebar3 proper # property tests
```

## See also

* [argon2_elixir](httpsi://github.com/riverrun/argon2_elixir), the elixir library upon which this one is based and whose nifs we use. It doesn't want to build in a rebar3 project.

## Copyright and License

Erlang code (c) 2017 James Laver, MIT LICENSE

This library contains code licensed under other licenses:

Argon2 is (c) Daniel Dinu and Dmitry Khovratovich, Apache 2.0 Licensed
BLAKE2 is (c) Samuel Neves, 2013-2015, CC0 1.0 Licensed
NIFs are (c) David Whitlock, Dual CC0 1.0/Apache 2.0 Licensed

