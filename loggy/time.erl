% introduce logical time to worker process

-module(time).
-compile(export_all).

% return an initial Lamport value
zero() -> 0.

% return the time T incremented by one
inc(Name, T) ->
  T + 1.

% merge two Lamport time stamps, take the maximum value
merge(Ti, Tj) when Ti > Tj  ->
  Ti;
merge(Ti, Tj) when Ti =< Tj  ->
  Tj.

% true if Ti is less than or equal to Tj
leq(Ti, Tj) when Ti =< Tj ->
  true;
leq(Ti, Tj) when Ti > Tj ->
  false.

% initiate a clock for the nodes with an initial Lamport time
clock(Nodes) ->
  lists:map(fun(Node)-> {Node, zero()} end, Nodes).

% return a clock that has been updated with Time
% given that we have received a log message from a node at a given time
update(Node, Time, Clock) ->
  UpdatedClock = lists:keyreplace(Node, 1, Clock, {Node, Time}),
  lists:keysort(2, UpdatedClock).

% is it safe to log an event that happened at Time
% is time lower than the elements in the list
safe(Time, [{_, T} | _]) ->
  leq(Time, T).
