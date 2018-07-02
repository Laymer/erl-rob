-module(jitterTest).
-export([sendMsg/0, sendLoop/1]).

sendMsg()->{msgHandler, remsh@X1} ! msg.
sendLoop(0) -> sendMsg();
sendLoop(N) when N > 0 -> sendMsg(), timer:sleep(10), sendLoop(N-1).
