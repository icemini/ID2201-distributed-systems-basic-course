% logger accepts events and prints them to the screen

-module(logger).
-export([start/1, stop/1]).

start(Nodes) ->
  spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
  Logger ! stop.

init(Nodes) ->
  loop(time:clock(Nodes), []).

% receives the log messages
loop(Clock, HoldBackQueue) ->
  receive
    {log, From, Time, Msg} ->
      UpdatedClock = time:update(From, Time, Clock),
      UpdatedHBQ = [{From, Time, Msg} | HoldBackQueue],   % add message to HBQ
      SortedHBQ = lists:keysort(2, UpdatedHBQ),
      NewHBQ = checkMessage(SortedHBQ, UpdatedClock, []), % safe to log?
      loop(UpdatedClock, NewHBQ);
    stop ->
      io:format("size of HBQ: ~w~n", [length(HoldBackQueue)]),
      ok
  end.

% print the log messages
log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

% check if a message is safe to log
checkMessage([], _, NewHBQ) ->
  NewHBQ;                        % rest of the messages that are still not safe to log
checkMessage([{From, Time, Msg} | Rest], Clock, NewHBQ) ->
  case time:safe(Time, Clock) of
    true ->
      log(From, Time, Msg),
      checkMessage(Rest, Clock, NewHBQ);
    false ->
      checkMessage(Rest, Clock, [{From, Time, Msg} | NewHBQ])   % add message that is not safe to log
  end.
