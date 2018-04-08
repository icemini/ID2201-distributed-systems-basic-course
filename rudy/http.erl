% Web server, HTTP parser

-module(http).
-compile(export_all).

% parsing a HTTP get request
% a request consist of: a request line, headers, carriage return (\r) line feed (\n) (CRLF) and an optional body
% each header is terminated by a CRLF (new line)
parse_request(R0) ->
    {Request, R1} = request_line(R0),
    {Headers, R2} = headers(R1),
    {Body, _} = message_body(R2),
    {Request, Headers, Body}.


% request line consist of a method (GET), a request URI and a http version
% all separated by space characters (32) and a CRLF (13,10) at the end
request_line([$G, $E, $T, 32 |R0]) ->
    {URI, R1} = request_uri(R0),
    {Ver, R2} = http_version(R1),
    [13,10|R3] = R2,
    {{get, URI, Ver}, R3}.

% parsing the URI
request_uri([32|R0])->
  {[], R0};
request_uri([C|R0]) ->
  {Rest, R1} = request_uri(R0),
  % returns the URI as a string
  {[C|Rest], R1}.

% parsing the http version (either 1.0 or 1.1)
http_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) ->
    {v11, R0};
http_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) ->
    {v10, R0}.

% parsing sequence of headers
headers([13,10|R0]) ->
  {[],R0};
headers(R0) ->
  {Header, R1} = header(R0),
  {Rest, R2} = headers(R1),
  {[Header|Rest], R2}.

% parsing individual header
header([13,10|R0]) ->
    {[], R0};
header([C|R0]) ->
    {Rest, R1} = header(R0),
    % returns the header
    {[C|Rest], R1}.

% return the body
message_body(R) ->
    {R, []}.

% generate an 200 OK response (all is OK)
ok(Body) ->
  "HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body.

% generate a GET request
% double \r\n, one to end the status line and one to end the header section
get(URI) ->
  "GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".
