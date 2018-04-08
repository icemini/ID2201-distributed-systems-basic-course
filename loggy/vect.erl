

-module(vect).
-compile(export_all).

% return an initial vector value
zero() -> [].

% return the vector time Time incremented by one
inc(Name, Time) ->
    case lists:keyfind(Name, 1, Time) of
         {Name, TimeStamp} ->
            lists:keyreplace(Name, 1, Time, {Name, TimeStamp + 1});
        false ->
            [{Name, 1}|Time]
    end.

% merge two Lamport time stamps, take the maximum value
merge([], Time) ->
    Time;
merge([{Name, Ti}|Rest], Time) ->
    case lists:keyfind(Name, 1, Time) of
        {Name, Tj} ->
            [{Name, erlang:max(Ti,Tj)}|merge(Rest, lists:keydelete(Name, 1, Time))];
        false ->
            [{Name, Ti}|merge(Rest, Time)]
    end.

% true if all entries in one vector clock is less than another
leq([], _) ->
    true;
leq([{Name, Ti}|Rest],Time) ->

    case lists:keyfind(Name, 1, lists:keysort(2, Time)) of  % sort since there cna be multiple entries with same worker in Time and we want to compare the smallest one
        {Name, Tj} ->
            if
                Ti =< Tj ->
                  leq(Rest, Time);
                true ->
                  false
            end;
        false ->
          false       %worker does not exist in the vector
    end.

% initiate a clock for the nodes with an initial Lamport time
clock(_) ->
  [].

% return a clock that has been updated with Time
% Time is the new Time vector and Clock is all the vectors
update(From, Time, Clock) ->
    Message = lists:keyfind(From, 1, Time),   % to receive the correct vector
    case lists:keyfind(From, 1, Clock) of
        {From, _} ->
            lists:keyreplace(From, 1, Clock, Message);
        false ->
            [Message | Clock]
    end.

% is it safe to log an event that happened at Time
% is time lower than the elements in the list
safe(Time, Clock) ->
  leq(Time, Clock).
