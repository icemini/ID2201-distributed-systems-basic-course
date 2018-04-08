% An interface is described by a name, a process reference and a process identifier

-module(intf).
-compile(export_all).

% return empty set of interfaces
new() -> [].

% add a new entry to the set and return a new set of interfaces
add(Name, Ref, Pid, Intf) ->
  [{Name, Ref, Pid}|Intf].

% remove an entry given a name of an interface
% return a new set of interfaces
remove(Name, Intf) ->
  lists:keydelete(Name, 1, Intf).

% find the process identifier given a name
lookup(Name, Intf) ->
  case lists:keyfind(Name, 1, Intf) of
    {_,_,Pid} -> {ok, Pid};
    false -> notfound
  end.

% find the reference given a name
ref(Name, Intf) ->
  case lists:keyfind(Name, 1, Intf) of
    {_,Ref,_}  -> {ok, Ref};
    false -> notfound
  end.

% find the name of an entry given a reference
name(Ref, Intf) ->
  case lists:keyfind(Ref, 2, Intf) of
    {Name,_,_}  -> {ok, Name};
    false -> notfound
  end.

% return a list with all names
list([]) -> [];
list([{Name, _, _}|T]) ->
  [Name | list(T)].

% send the message to all interfaces
broadcast(Message, Intf) ->
    lists:foreach(fun({_Name, _Ref, Pid}) -> Pid ! Message end, Intf).
