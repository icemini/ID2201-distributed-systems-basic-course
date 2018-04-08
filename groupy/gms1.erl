% handling the starting of a single node first version

-module(gms1).
-compile(export_all).

-define(arghh, 100).

% initiate a process that is the first node in a group
start(Id) ->
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Self) end)}.

init(Id, Master) ->
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
            slave(Id, Master, Leader, Slaves, Group)  % initial state is slave
    end.

% leader procedure
leader(Id, Master, Slaves, Group) ->
    receive
    % message from master or peer node
    {mcast, Msg} ->
        bcast(Id, {msg, Msg}, Slaves),  % send the message to all the slaves
        Master ! Msg,                   % send message to master
        leader(Id, Master, Slaves, Group);
    % request from node to join group
    {join, Wrk, Peer} ->
        % add the new node to the slaves and the group
        Slaves2 = lists:append(Slaves, [Peer]),
        Group2 = lists:append(Group, [Wrk]),
        bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2), % broadcast new view
        Master ! {view, Group2},    % send new view of group to master
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
      stop ->
          ok
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
