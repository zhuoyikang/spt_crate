-module(spt_atom).
-export([atom_suffix/2, atom_prefix/2]).

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
