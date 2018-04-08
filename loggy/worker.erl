% the worker waits for a while and sends a message to one of its peers
% while waiting it can receive messages from peers

-module(worker).
-export([start/5, stop/1, peers/2]).

% a worker is given a unique name and access to the logger
start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
    Worker ! stop.

% calls the worker with the peers
init(Name, Log, Seed, Sleep, Jitter) ->
    random:seed(Seed, Seed, Seed),
    receive
        {peers, Peers} ->
            loop(Name, Log, Peers, Sleep, Jitter, time:zero());
        stop ->
            ok
    end.

% inform the worker of the peers
peers(Wrk, Peers) ->
  Wrk ! {peers, Peers}.

% worker process
loop(Name, Log, Peers, Sleep, Jitter, LampTime)->

    Wait = random:uniform(Sleep),   % wait for message from one of its peers

    receive
        {msg, Time, Msg} ->
            % add a timestamp to the message
            TimeStamp = time:inc(Name, time:merge(LampTime, Time)),
            % log the message, send it to the logger
            Log ! {log, Name, TimeStamp, {received, Msg}},
            loop(Name, Log, Peers, Sleep, Jitter, TimeStamp);
        stop ->
            ok;
        Error ->
            Log ! {log, Name, time, {error, Error}}
    % if no message has arrived within Wait time
    % select a peer process to send a message to
    after Wait ->
            Selected = select(Peers),
            % add timestamp
            TimeStamp = time:inc(Name, LampTime),
            Message = {hello, random:uniform(100)}, %msg has unique value
            Selected ! {msg, TimeStamp, Message},
            jitter(Jitter),
            Log ! {log, Name, TimeStamp, {sending, Message}},
            loop(Name, Log, Peers, Sleep, Jitter, TimeStamp)
    end.

% returns a random peer/worker
select(Peers) ->
  lists:nth(random:uniform(length(Peers)), Peers).

% random delay between sending the message to the peer and informing the logger
jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).
