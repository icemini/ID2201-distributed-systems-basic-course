% keeps track of what messages has been sent

-module(hist).
-compile(export_all).

% return a new history
% messages from Name will be seen as old
new(Name) ->
  [{Name, 0}].

% check if message number N from Node is old or new
% replace if new
update(Node, N, History) ->
    case lists:keyfind(Node, 1, History) of

	{Node, N2} ->

	    case N =< N2 of
    		true ->
    		    old;
    		false ->
    		    {new, lists:keyreplace(Node, 1, History, {Node, N})}
	    end;
      	false ->
      	    {new, [{Node, N} | History]}
    end.
