% program that waits for incoming requests, delivers a reply and then terminates
% usage: erl -> c(rudy) -> rudy:start(8080).

-module(rudy).
-compile(export_all).

% start several processes to listen for incoming requests
start_proc(0, _) -> ok;
start_proc(N, Listen) ->
  spawn(rudy, handler, [Listen]),
  start_proc(N-1, Listen).

% starts the program and registers the process under the name "rudy"
start(Port) ->
    register(rudy, spawn(fun() -> init(Port) end)).

% exit process registrered under rudy
stop() ->
    exit(whereis(rudy), "time to die").

% initialize server, takes a port number, opens a listening socket and passes the socket to handler/1
init(Port) ->
  % see the bytes as a list of integers
  Opt = [list, {active, false}, {reuseaddr, true}],
  case gen_tcp:listen(Port, Opt) of                    % opens a listening socket
      {ok, Listen} ->
        start_proc(10, Listen),
        handler(Listen),
      gen_tcp:close(Listen),                           % close socket
      ok;
      {error, Error} ->
        error
  end.

% listens to socket for incoming connection and passes the connection to request/1
handler(Listen) ->
  case gen_tcp:accept(Listen) of                       % listen to socket for incoming connection requests
      {ok, Client} ->                                  % connection has been established by the client
          request(Client),
          handler(Listen);
      {error, Error} ->
          error
  end.

% read GET request from client connection and parse it using http parser and reply
request(Client) ->
    Recv = gen_tcp:recv(Client, 0),                     % read input from client connection
    case Recv of
        {ok, Str} ->
            Request = http:parse_request(Str),
            Response = reply(Request),
            gen_tcp:send(Client, Response);             % send response to the client
        {error, Error} ->
            io:format("rudy: error: ~w~n", [Error])
    end,
    gen_tcp:close(Client).

% decide on a reply, delay to simulate file handling, server side scripting etc
reply({{get, URI, _}, _, _}) ->
  timer:sleep(40),
  http:ok("Everything works").
