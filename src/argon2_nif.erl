-module(argon2_nif).

-export([hash_nif/10, verify_nif/3, error_nif/1, encodedlen_nif/6]).
-on_load(init/0).

-define(APPNAME, argon2).
-define(LIBNAME, argon2_nif).

hash_nif(_TCost, _MCost, _Parallelism, _Password, _Salt, _Raw, _HashLen, _EncodedLen, _Argon2Type, _Argon2Version) ->
  not_loaded(?LINE).
verify_nif(_Hash, _Password, _Argon2Type) ->
  not_loaded(?LINE).
error_nif(_Code) ->  
  not_loaded(?LINE).
encodedlen_nif(_TCost, _MCost, _Parallelism, _SaltLen, _HashLen, _Argon2Type) ->  
  not_loaded(?LINE).

init() ->
  SoName =
    case code:priv_dir(?APPNAME) of
      {error, bad_name} ->
	case filelib:is_dir(filename:join(["..", priv])) of
	  true ->
	    filename:join(["..", priv, ?LIBNAME]);
	  _ ->
	    filename:join([priv, ?LIBNAME])
	end;
      Dir ->
	filename:join(Dir, ?LIBNAME)
    end,
  erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).

