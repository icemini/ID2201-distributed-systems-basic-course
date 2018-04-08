% benchmark program for the logger and worker

-module(vecttest).
-export([run/2]).

run(Sleep, Jitter) ->

    % start logger and workers
    Log = vectlogger:start([john, paul, ringo, george]),
    A = vectworker:start(john, Log, 13, Sleep, Jitter),
    B = vectworker:start(paul, Log, 23, Sleep, Jitter),
    C = vectworker:start(ringo, Log, 36, Sleep, Jitter),
    D = vectworker:start(george, Log, 49, Sleep, Jitter),

    % inform the workers of their peers
    vectworker:peers(A, [B, C, D]),
    vectworker:peers(B, [A, C, D]),
    vectworker:peers(C, [A, B, D]),
    vectworker:peers(D, [A, B, C]),

    timer:sleep(5000),
    vectlogger:stop(Log),
    vectworker:stop(A),
    vectworker:stop(B),
    vectworker:stop(C),
    vectworker:stop(D).
