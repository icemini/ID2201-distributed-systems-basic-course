
-module(vectlogger).
-compile(export_all).

start(Nodes) ->
  spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
  Logger ! stop.

init(Nodes) ->
  loop(vect:clock(Nodes), []).

% receives the log messages
loop(Clock, HoldBackQueue) ->
  receive
    {log, From, VectTime, Msg} ->
      UpdatedClock = vect:update(From, VectTime, Clock),
      UpdatedHBQ = [{From, VectTime, Msg} | HoldBackQueue],
      NewHBQ = checkMessage(UpdatedHBQ, UpdatedClock, []),  % safe to log?
      loop(UpdatedClock, NewHBQ);
    stop ->
      %io:format("size of HBQ: ~w~n", [length(HoldBackQueue)]),
      ok
  end.

% print the log messages
log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

% checck if a message is safe to log
checkMessage([], _, NewHBQ) ->
  NewHBQ;
checkMessage([{From, VectTime, Msg} | Rest], Clock, NewHBQ) ->
  case vect:safe(VectTime, Clock) of
    true ->
      log(From, VectTime, Msg),
      checkMessage(Rest, Clock, NewHBQ);
    false ->
      checkMessage(Rest, Clock, [{From, VectTime, Msg} | NewHBQ])
  end.
