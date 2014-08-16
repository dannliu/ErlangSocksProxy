%% Start a server to listen on port 1080, which later will establish connection from  SOCKS client


%% T represent the state of the sock connection, init -> hello -> reply

%% Todo: Add cache for the domain host
%% Add expired time to the IP cache

-module(entry).
-export([start_socks/0,start_socks/1]).
-import(error_logger,[info_msg/1,info_msg/2,error_msg/1,error_msg/2]).

-define(INIT, init).
-define(HELLO, hello).
-define(CONNECTED, connected).
-define(SOCKS_PORT,douban).

-type state() :: init|hello|connected.

-spec loop(LocalSocket, State, RemoteSocket) -> any() when
    LocalSocket :: gen_tcp:socket(),
    State :: state(),
    RemoteSocket :: gen_tcp:socket().

-spec handle_connection_request(LocalSocket, RemoteSocket, State, Packet) -> {state(),gen_tcp:socket()} when
    LocalSocket :: gen_tcp:socket(),
    RemoteSocket :: gen_tcp:socket(),
    State :: state(),
    Packet :: binary().

start_socks() ->
    start_socks(?SOCKS_PORT).

start_socks([Port|_]) ->
    try
        start_socks(list_to_integer(Port))
    catch
        error:Reason -> error_msg("start socks proxy failed, reason = ~p~n",[Reason])
    end;
    
start_socks(Port) ->
    init(),
    {Result, Listen} = gen_tcp:listen(Port,[binary,{active,false},{packet,0}]),
    case Result of
        ok ->  new_connection(Listen);
        error -> io:format("ERROR, failed to listen on port ~p as ~p",[Port,Listen])
    end.

%% Initialize the Host to IP cache
init() ->
    Pid = spawn(fun() -> ip_cache(#{}) end),
    register(cache, Pid).

new_connection(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> loop(Socket, ?INIT, undefined) end),
    new_connection(Listen).

loop(LS, ST, RS) ->
    {Result, Packet} = gen_tcp:recv(LS, 0, infinity),
    case Result of
        ok -> 
            {State, RemoteSocket} = handle_connection_request(LS, RS, ST, Packet),
            loop(LS, State, RemoteSocket);
        error -> io:format("loop, Reason = ~p~n",[Packet]),gen_tcp:close(LS)
    end.

handle_connection_request(LS, RS, ST, Packet) ->
    case ST of
        ?INIT      -> handle_init_request(LS, Packet);
        ?HELLO     -> handle_hello_request(LS, Packet);
        ?CONNECTED -> handle_reply_request(RS, Packet);
        _Any -> io:format("Error for handle connection request"), error
    end.

handle_init_request(LS, Packet) ->
    send_packet(LS, <<5, 0>>),
    {?HELLO, undefined}.

handle_hello_request(LS, Packet) ->
    %%fetch the remote addr and remote port from the data.
    send_packet(LS, <<5,0,0,1,0,0,0,0,16,16>>),
    {Addr, Port} = parse_destination_addr(Packet),
    %%todo connection may be failed here, do we need to reconnect?
    {Result, RS} = gen_tcp:connect(Addr, trs_port(Port),[binary, {active,false}, {packet,0}]),
    spawn(fun() -> handle_remote_data(RS, LS) end),
    {?CONNECTED, RS}.

handle_reply_request(RS, Packet) ->
    send_packet(RS, Packet), 
    {?CONNECTED, RS}. 

handle_remote_data(RS, LS) ->
    {Result, Packet} = gen_tcp:recv(RS,0,infinity),
    case Result of
        ok -> 
            send_to_local(LS, Packet),
            handle_remote_data(RS, LS);
        error -> io:format("##########~p~n",[Packet]),gen_tcp:close(RS)
    end.


%% When the proxy retrive data from the remote,  send the data to the local socket.
%% Implement a queue to send the data sequentially.
send_to_local(LS, Packet) ->
    Pid = get(LS),
    if 
        Pid == undefined ->
            Npid = spawn(fun() -> async_send_to_local() end),
            put(LS, Npid),
            Npid ! {ok, LS, Packet};
        true ->
            Pid ! {ok, LS, Packet}
    end.

async_send_to_local() ->
    receive
        {ok, LS, Packet} -> 
            send_packet(LS, Packet),
            async_send_to_local()
    end.


send_packet(Socket,Packet) ->
    gen_tcp:send(Socket,Packet).


parse_destination_addr(Packet) ->
    <<_:3/binary,T,_/binary>> = Packet,
    %%io:format("The type of request destination is ~B~n",[T]),
    case T of
       16#01 -> 
            <<_:3/binary,T,Addr:4/binary,Port:2/binary>> = Packet, 
            {list_to_tuple(Addr), Port};
       16#03 ->
            <<_:3/binary,T,N,URL:N/binary,Port:2/binary>> = Packet,
            {url_to_ip(binary_to_list(URL)), Port};
       16#04 ->
            <<_:3/binary,T,Addr:16/binary,Port:2/binary>> = Packet,
            {list_to_tuple(Addr), Port}
    end.

url_to_ip(URL) ->
    %%io:format("The URL = ~p need to transfer to ip ~n", [URL]),
    IP = get_ip(URL),
    info_msg("######IP = ~p~n",[IP]),
    case IP of
        undefined ->
            {S,{hostent,_,_,_,_,Addrs}} = inet:gethostbyname(URL),
            case S of
                ok ->
                    [Addr|_] = Addrs,
                    store_ip(URL, Addr),
                    Addr;
                error ->
                    io:format("Failed to get ip address, URL = ~p",[URL]),
                    {{},0}
            end;
        IP -> info_msg("The cahced ip for host ~p is ~p~n",[URL, IP]), IP
    end.


trs_port(Port) ->
    <<H1,H2>> = Port,
    H1 * 256 + H2.


ip_cache(IP_map) ->
    receive
        {'get',Host, Pid} -> 
            IP = maps:get(Host,IP_map,undefined),
            Pid ! {self(), IP},
            ip_cache(IP_map);
        {'remove', Host} ->
            M2 = maps:remove(Host, IP_map),
            ip_cache(M2);
        {'cache', Host, IP} ->
            M2 = maps:put(Host,{IP, tis()},IP_map),
            ip_cache(M2)
    end.

store_ip(Host, IP) ->
    Pid = whereis(cache),
    Pid ! {'cache',Host, IP}.

get_ip(Host) ->
    Pid = whereis(cache),
    Pid ! {'get', Host , self()},
    receive
        {Pid,undefined} -> undefined;
        {Pid, {IP, Time}} ->
            CurrentTime = tis(),
            if 
                CurrentTime - Time > 30 ->
                    Pid ! {'remove', Host},
                    undefined;
               true -> IP
            end;
        _Any -> info_msg("error here ~p~n",[_Any])
    end.

tis() ->
    calendar:datetime_to_gregorian_seconds(calendar:local_time()).
    
