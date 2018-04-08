% handling the starting of a single node second version
% handling failure

-module(gms2).
-compile(export_all).

-define(timeout, 1000).
-define(arghh, 100).

% initiate a process that is the first node in a group
start(Id) ->
    Rnd = random:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun()-> init_leader(Id, Rnd, Self) end)}.

init_leader(Id, Rnd, Master) ->
    random:seed(Rnd, Rnd, Rnd),
    leader(Id, Master, [], [Master]).

start(Id, Grp) ->
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Grp, Self) end)}.

% start node that should join the group
init(Id, Grp, Master) ->
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        % invitation of new view after joining group
        {view, [Leader|Slaves], Group} ->
            Master ! {view, Group},
            erlang:monitor(process, Leader),    % monitor the leader
            slave(Id, Master, Leader, Slaves, Group)  % initial state is slave

    % timeout when waiting for an invitation to join group
    % if the leader has crashed
    after ?timeout ->
            Master ! {error, "no reply from leader"}
    end.

% leader procedure
leader(Id, Master, Slaves, Group) ->
    receive
    % message from master or peer node
    {mcast, Msg} ->
        bcast(Id, {msg, Msg}, Slaves),
        Master ! Msg,
        leader(Id, Master, Slaves, Group);
    % request from node to join group
    {join, Wrk, Peer} ->
        % add the new node to the slaves and the group
        Slaves2 = lists:append(Slaves, [Peer]),
        Group2 = lists:append(Group, [Wrk]),
        bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2),
        Master ! {view, Group2},
        leader(Id, Master, Slaves2, Group2);

    stop ->
        ok
    end.

% slave procedure
% accepts messages from master and leader
slave(Id, Master, Leader, Slaves, Group) ->
  receive
    % request from master to multicast message
    {mcast, Msg} ->
        Leader ! {mcast, Msg},    % forward it to the leader
        slave(Id, Master, Leader, Slaves, Group);
    % request from master to allow new node to join the group
    {join, Wrk, Peer} ->
        Leader ! {join, Wrk, Peer},
        slave(Id, Master, Leader, Slaves, Group);
    % multicasted message from leader
    {msg, Msg} ->
        Master ! Msg,
        slave(Id, Master, Leader, Slaves, Group);
    % multicasted view from leader
    {view, [Leader|Slaves2], Group2} ->
        Master ! {view, Group2},
        slave(Id, Master, Leader, Slaves2, Group2);
      % when the leader dies
      {'DOWN', _Ref, process, Leader, _Reason} ->
          election(Id, Master, Slaves, Group);
      stop ->
          ok
  end.

% election procedure
election(Id, Master, Slaves, [_|Group]) ->
    Self = self(),
    case Slaves of
        % select first node as leader
        [Self|Rest] ->
            bcast(Id, {view, Slaves, Group}, Rest),
            Master ! {view, Group},
            leader(Id, Master, Rest, Group);
        % the first node in the list is the leader
        [Leader|Rest] ->
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, Rest, Group)
    end.

% broadcasts a message to all the nodes
% see if a node will crash
bcast(Id, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

% a random crash
% an arghh value of 100 means that the system will crash in average once in a hundred
crash(Id) ->
  case random:uniform(?arghh) of
      ?arghh ->
          io:format("leader ~w: crash~n", [Id]),
          exit(no_luck);
      _ ->
          ok
  end.
