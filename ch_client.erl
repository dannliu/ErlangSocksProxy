%ndle_remote_connection% The client is mainly for chinese user as the GFW will block the socks proxy
%% The client will encry the data browser or any end application send to the proxy,
%% then forward to the socks proxy server

-module(ch_client).

-define(LOCAL_PORT, 1090).
-define(PROXY_IP, {58,96,173,81}).
-define(PROXY_PORT, 1080).
-define(TIME_OUT, 20000).

-import(error_logger, [error_msg/1, error_msg/2, info_msg/1, info_msg/2]).
-export([start/0,start/1,handle_connection/1,handle_connection/2,handle_remote_connection/2]).

start() ->
    start(?LOCAL_PORT). 

start(Port) ->
    {Result, Socket} = gen_tcp:listen(Port, [binary, {active, false}]),
    case Result of 
        ok -> new_connection(Socket);
        error -> error_msg("Faile to the listen to the port = ~p, Reason = ~p~n",[Port, Socket])
    end.

new_connection(ListenSocket) ->
    {Result, Socket} = gen_tcp:accept(ListenSocket),
    case Result of
        ok -> 
            spawn(?MODULE, handle_connection,[Socket]),
            new_connection(ListenSocket);
        error -> error_msg("Failed to accept a new connection, Reason = ~p~n",[Socket])
    end.

handle_connection(Socket) ->
    {Result, RemoteSocket} = gen_tcp:connect(?PROXY_IP, ?PROXY_PORT, [binary, {active, false}, {send_timeout, ?TIME_OUT}], ?TIME_OUT),
    case Result of
        ok -> 
            spawn(?MODULE, handle_remote_connection, [RemoteSocket, Socket]),
            handle_connection(Socket, RemoteSocket);
        error -> 
            error_msg("Failed to connect to the remote server, Reason = ~p~n", [RemoteSocket]), 
            gen_tcp:close(Socket)
    end.

handle_connection(Socket, RemoteSocket) ->
    {Result, Packet} = gen_tcp:recv(Socket, 0, ?TIME_OUT),
    case Result of
        ok ->
            EncryptedPacket = << <<bnot X>> || <<X>> <= Packet>>,
            gen_tcp:send(RemoteSocket, EncryptedPacket),
            handle_connection(Socket, RemoteSocket);
        error ->
            case Packet of
                closed -> ok;
                _Any -> error_msg("Receive data error, Reason = ~p~n",[Packet])
            end
    end.

handle_remote_connection(RemoteSocket, Socket) ->
    {Result, Packet} = gen_tcp:recv(RemoteSocket, 0, ?TIME_OUT),
    %%Decryt the data
    case Result of
        ok ->
            gen_tcp:send(Socket, Packet),
            handle_remote_connection(RemoteSocket, Socket);
        error ->
            case Packet of
                closed -> ok;
                _Any -> error_msg("Receive data error, Reason = ~p~n",[Packet])
            end
    end.

    
    
