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
        error -> io:format("ERROR, failed to listen on port ~p as ~p",[Port,Listen])
    end.

new_connection(Listen) ->
    {ok, S} = gen_tcp:accept(Listen),
    spawn(fun() -> loop(S, ?INIT, {}) end),
    new_connection(Listen).

%% S: Socket
%% T: Connection state
%% A: Addr with format of {IP, Port}.
loop(S, T, A) ->
    %%io:format("start to receive data from socket ~p, state = ~p~n", [S, T]),
    {C, Data} = gen_tcp:recv(S, 0, infinity),
    case C of
        ok -> io:format("received data  = ~p~n",[Data]),
            {State, Addr} = handle_connection_request(S, T, Data, A),
            loop(S, State, Addr);
        error -> io:format("loop, Reason = ~p~n",[Data]),gen_tcp:close(S)
    end.

handle_connection_request(S, T, Data, Addr) ->
    case T of
        ?INIT -> handle_init_request(S, Data);
        ?HELLO -> handle_hello_request(S, Data);
        ?CONNECTED -> handle_reply_request(S, Addr, Data);
        Any -> io:format("Error for handle connection request"), error
    end.

handle_init_request(S, Data) ->
    %%io:format("S1"), 
    send_data(S, <<5, 0>>),
    {?HELLO, {}}.

handle_hello_request(S, Data) ->
    %%io:format("S2"),
    %%fetch the remote addr and remote port from the data.
    send_data(S, <<5,0,0,1,0,0,0,0,16,16>>),
    {Addr, Port} = parse_destination_addr(Data),
    io:format("Addr = ~p~n", [Addr]),
    io:format("Port = ~p~n", [Port]),
    {?CONNECTED,{Addr,trs_port(Port)}}.

handle_reply_request(S, Addr, Data) ->
    {IP, Port} = Addr,
    handle_remote_data(IP, Port, Data, S),
    {?CONNECTED,Addr}. 

handle_remote_data(Addr, Port, RequestData, LocalSocket) ->
    %%io:format("Start to fetch data from actual sever ~p ~p ~p ~n",[Addr, Port, RequestData]),
    {T, S} = gen_tcp:connect(Addr, Port,[binary, {active,false}, {packet,0}]),
    case T of
        ok ->
            gen_tcp:send(S,binary_to_list(RequestData)),
            receive_data(S, LocalSocket);
        error ->
            io:format("handle_remote_data, Reason = ~p, Addr = ~p, Port = ~p~n",[S,Addr,Port])
    end.


receive_data(S, LocalSocket) ->
    {C, Data} = gen_tcp:recv(S,0,infinity),
    io:format("The data return by the remote server ~p~n",[Data]),
    case C of
        %%ok -> spawn(fun()-> gen_tcp:send(LocalSocket, Data) end), receive_data(S, LocalSocket);
        ok -> gen_tcp:send(LocalSocket, Data), receive_data(S, LocalSocket);
        error -> io:format("##########~p~n",[Data]),gen_tcp:close(S)
    end.

send_data(Socket,Data) ->
    gen_tcp:send(Socket,Data).

%% When the proxy retrive data from the remote,
%% send the data to the local socket.
send_to_local(LS, Packet) ->
    receive
    end.


parse_destination_addr(Data) ->
    <<_:3/binary,T,_/binary>> = Data,
    %%io:format("The type of request destination is ~B~n",[T]),
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
    %%io:format("The URL = ~p need to transfer to ip ~n", [URL]),
    {S,{hostent,_,_,_,_,Addrs}} = inet:gethostbyname(URL),
    case S of
        ok ->
            [Addr|_] = Addrs,
            Addr;
        error ->
            io:format("Failed to get ip address, URL = ~p",[URL]),
            {{},0}
    end.


trs_port(Port) ->
    <<H1,H2>> = Port,
    H1 * 256 + H2.

