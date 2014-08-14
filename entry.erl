%% Start a server to listen on port 1080, which later will establish connection from  SOCKS client


%% T represent the state of the sock connection, init -> hello -> reply

-module(entry).
-export([start_socks/0,start_socks/1]).

-define(INIT, init).
-define(HELLO, hello).
-define(CONNECTED, connected).

start_socks() ->
    start_socks(1090).

start_socks(Port) ->
    {C, Listen} = gen_tcp:listen(Port,[binary,{active,false},{packet,0}]),
    case C of
        ok ->  new_connection(Listen);
        error -> io:format("Error")
    end.

new_connection(Listen) ->
    {ok, S} = gen_tcp:accept(Listen),
    spawn(fun() -> loop(S, ?INIT) end),
    new_connection(Listen).

loop(S, T) ->
    io:format("test"),
    io:format("start to receive data from socket ~p, state = ~p~n", [S, T]),
    {C, Data} = gen_tcp:recv(S, 0, infinity),
    case C of
        ok -> io:format("received data  = ~p~n",[Data]),
        State = handle_connection_request(S, T, Data),
        loop(S, State);
        error -> io:format("Failed~n")
    end.

handle_connection_request(S, T, Data) ->
    case T of
        ?INIT -> handle_init_request(S, Data);
        ?HELLO -> handle_hello_request(S, Data);
        {?CONNECTED,Addr,Port} -> handle_reply_request(S, Data);
        Any -> io:format("Error for handle connection request"), error
    end.

handle_init_request(S, Data) ->
    io:format("prepare to replay the hello data"), 
    send_data(S, <<5, 0>>),
    ?HELLO.

handle_hello_request(S, Data) ->
    %%fetch the remote addr and remote port from the data.
    send_data(S, <<5,0,0,1,0,0,0,0,16,16>>),
    {Addr, Port} = parse_destination_addr(Data),
    io:format("Addr = ~p~n", [Addr]),
    io:format("Port = ~p~n", [Port]),
    {?CONNECTED,Addr,Port}.

handle_reply_request(S, Data) ->
    io:format("Start to fetch data from actual sever"),
    ?CONNECTED. 

handle_remote_data(Addr, Port, RequestData, LocalSocket) ->
    {ok, S} = gen_tcp:connect(Addr, Port,[binary, {packet,0}]),
    gen_tcp:send(S,RequestData),
    Response = receive_data(S,[]),
    gen_tcp:send(LocalSocket, Response).


receive_data(S, D) ->
    {C, Data} = gen_tcp:recv(S,0,20),
    case C of
        ok -> receive_data(S,[Data|D]);
        error -> list_to_binary(D)
    end.

send_data(Socket,Data) ->
    gen_tcp:send(Socket,Data).


parse_destination_addr(Data) ->
    <<_:3/binary,T,_/binary>> = Data,
    io:format("The type of request destination is ~B~n",[T]),
    case T of
       16#01 -> 
            <<_:3/binary,T,Addr:4/binary,Port:2/binary>> = Data, 
            {list_to_tuple(Addr), Port};
       16#03 ->
            <<_:3/binary,T,N,URL:N/binary,Port:2/binary>> = Data,
            {url_to_ip(binary_to_list(URL)), Port};
       16#04 ->
            <<_:3/binary,T,Addr:16/binary,Port:2/binary>> = Data,
            {list_to_tuple(Addr), Port}
    end.

url_to_ip(URL) ->
    io:format("The URL = ~p need to transfer to ip ~n", [URL]),
    {ok,{hostent,_,_,_,_,L3}} = inet:gethostbyname(URL),
    [Addr|_] = L3,
    Addr.


