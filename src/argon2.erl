-module(argon2).

-export([gen_salt/0, verify/2, hash/1, hash/2, hash/3]).
-export_type([argon2type/0]).

%%% public api

-type argon2type() :: argon2id | argon2i | argon2d.

-spec gen_salt() -> binary().
gen_salt() ->
  crypto:strong_rand_bytes(16).

-spec verify(binary(), binary()) -> boolean().
verify(Password,Hash) ->
  H = binary:bin_to_list(Hash),
  case argon2_nif:verify_nif(H, Password, type(Hash)) of
    0 -> true;
    _ -> false
  end.

-spec hash(binary()) -> binary().
hash(Password) ->
  hash(Password, gen_salt()).

-spec hash(binary(), binary()) -> binary().
hash(Password, Salt) ->
  hash(Password, Salt, #{}).

-spec hash(binary(), binary(), map()) -> binary().
hash(Password, Salt, Opts) ->
  Opts2={Time, Mem, Par, Len, Type} = opts(Opts),
  ELen = argon2_nif:encodedlen_nif(Time, Mem, Par, byte_size(Salt), Len, Type),
  Ret = argon2_nif:hash_nif(Time, Mem, Par, Password, Salt, 0 , Len, ELen, Type, 0),
  result(Ret, Opts2).

%%% internals

-spec type(argon2type() | binary()) -> pos_integer().
type(argon2id) -> 2;
type(argon2i)  -> 1;
type(argon2d)  -> 0;
type(<<"$argon2id$", _/binary>>) -> 2;
type(<<"$argon2i$",  _/binary>>) -> 1;
type(<<"$argon2d$",  _/binary>>) -> 0.

-spec opts(map()) -> {pos_integer(),pos_integer(),pos_integer(),pos_integer(), argon2type()}.
opts(Opts) ->
  {maps:get(time_cost, Opts, 6),
   maps:get(memory_cost, Opts, 16),
   maps:get(parallelism, Opts, 1),
   maps:get(length, Opts, 32),
   type(maps:get(type, Opts, argon2id))}.

result(Result, _) when is_integer(Result) ->
  erlang:error(binary:list_to_bin(argon2_nif:error_nif(Result)));
result({[], Encoded}, _) -> binary:list_to_bin(Encoded).

