-module(spt_atom).
-export([atom_suffix/2, atom_prefix/2]).
-export([binary_to_term/1, term_to_binary/1]).
-export([mysql_escape/1]).

atom_suffix(Table, Suffix) when is_list(Suffix)->
  L = atom_to_list(Table) ++ "_" ++ Suffix,
  list_to_atom(L);

atom_suffix(Table, Suffix) ->
  L = atom_to_list(Table) ++ "_" ++ atom_to_list(Suffix),
  list_to_atom(L).

atom_prefix(Key, Prefix) when is_list(Prefix)->
  L = Prefix ++ "_" ++ atom_to_list(Key),
  list_to_atom(L);
atom_prefix(Key, Prefix) ->
  L = atom_to_list(Prefix) ++ "_" ++ atom_to_list(Key),
  list_to_atom(L).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% term和binary转换.

binary_to_term(<<Bin/binary>>) ->
  String = erlang:binary_to_list(Bin),
  TmpString1 = string:strip(String, both, $.),
  TmpString = string:strip(TmpString1, both, $"),
  NewString = TmpString ++ ".",
  {ok, Tokens,_EndLine} = erl_scan:string(NewString),
  case erl_parse:parse_exprs(Tokens) of
    {ok, AbsForm} -> {value, Value,_Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()), Value;
    _ -> io:format("string error ~p~n", [String]), ok
  end.

term_to_binary(Term) ->
  R = io_lib:format("~p", [Term]),
  S = lists:flatten(R),
  list_to_binary(S).



%% 对binary进行escape操作.
mysql_escape(<<Binary/binary>>) ->
  binary:replace(Binary, [ <<"'">>,<<"\"">>], <<"\\">>, [global, {insert_replaced,1}]).
