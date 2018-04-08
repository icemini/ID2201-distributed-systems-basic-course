% module for running the program

-module(test).
-compile(export_all).


start() ->

  Com1 = 'sweden@130.229.153.114',
  Com2 = 'finland@130.229.183.250',

    % start the routers

    routy:start(r1, lund),
    routy:start(r2, stockholm),

    timer:sleep(100),

    % connect them to each other

    r1 ! {add, stockholm, {r2, Com1}},
    timer:sleep(100),
    r2 ! {add, lund, {r1, Com1}},
    timer:sleep(100),

    r1 ! broadcast,
    timer:sleep(100),
    r2 ! broadcast,
    timer:sleep(100),

    r1 ! update,
    timer:sleep(100),
    r2 ! update.

stop() ->
    routy:stop(lund),
    routy:stop(stockholm).
  %  routy:stop(r3).
