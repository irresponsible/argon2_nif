-module(argon2).

-export([gen_salt/0, compare/2, hash/1, hash/2]).

gen_salt() ->
  crypto:strong_rand_bytes(16).

compare(Password,Hash) ->
  ok.

hash(Password) ->
  hash(Password, gen_salt()).

hash(Password,Salt) ->
  ok.
