-module(discovery_server).

-behaviour(gen_server).

%% gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, code_change/3, terminate/2]).

%% API exported functions.
-export([start_link/4]).

%% API record definition.
-record(state, 
	{addr,
	 ip,
	 interval,
	 send_sock = undefined,
	 send_port,
	 recv_sock = undefined,
	 recv_port
	 }).

%% UDP Message Header definition
-define(MSG_HEADER, "DISCOVER ").

%% ==== API functions.

start_link(Interface,InboundPort,OutboundPort, Interval) ->
    ParamsAsList = [Interface,InboundPort,OutboundPort, Interval],
    discovery_log:debug(?MODULE, start_link, ParamsAsList),
    gen_server:start_link({local, ?MODULE}, ?MODULE, ParamsAsList, []).

%% ==== gen_server callback functions.

init([_, _, _, Interval] = Param) ->
    discovery_log:debug(?MODULE, init, Param),
    erlang:send_after(Interval, self(), discover),
    init_state(Param).    

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(discover, State=#state{interval=Interval}) ->
    send_broadcast_packet(State),
    erlang:send_after(Interval, self(), discover),
    {noreply, State};
handle_info({udp, Socket, Ip, _Inport, _Packet}, State=#state{recv_sock = Socket, ip=Ip}) ->
    {noreply, State};
handle_info({udp, Socket, Ip, InPort, Packet}, State=#state{recv_sock = Socket}) ->
    {noreply, process_packet(Packet, Ip, InPort, State)};
handle_info(Msg, State) ->
    discovery_log:debug(?MODULE, handle_info, Msg),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


%% ==== API Private functions.

%% initializes state.
init_state([Interface, InboundPort, OutboundPort, Interval]) ->
    {ok, Addr} = get_broadcast_addr(Interface),
    {ok, Ip}   = get_if_info(addr, Interface), 
    {ok, SendSock} = gen_udp:open(InboundPort, [{broadcast, true}]),
    {ok, RecvSock} = gen_udp:open(OutboundPort),

    State = #state{
	       addr = Addr,
	       ip   = Ip,
	       interval  = Interval,
	       send_port = InboundPort,
	       recv_port = OutboundPort,
	       send_sock = SendSock,
	       recv_sock = RecvSock
	      },
    {ok, State}.

%% sends broadcast packet.
send_broadcast_packet(_State=#state{send_sock=SendSocket, 
				    addr=Ip, recv_port = OutPort}) ->
    NodeString = atom_to_list(node()),
    Time = seconds(),
    Time64 = <<Time:64>>,
    Mac = mac([Time64, NodeString]),
    Msg = [?MSG_HEADER, Mac, " ", Time64, " ", NodeString],
    gen_udp:send(SendSocket, Ip, OutPort, Msg).

%% get information about given interface.
get_if_info(Key, Interface) ->
    Result = case inet:ifget(Interface, [Key]) of
		 {ok, [{Key, Ip}]} ->
		     {ok, Ip};
		 _ ->
		     {error, no_ifinfo_get}
	     end,
    discovery_log:debug(?MODULE, get_if_info, [Key, Interface, Result]),
    Result.

get_broadcast_addr(Interface) ->
    get_if_info(broadaddr, Interface).
   
%% gets seconds.
seconds() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

%% creates sha mac from (cookie + Message).
mac(Message) ->
    Key = crypto:hash(sha, erlang:term_to_binary(erlang:get_cookie())),
    crypto:hmac(sha, Key, Message).

%% processes incoming UDP packet.
process_packet(?MSG_HEADER ++ Rest, _Ip, _InPort, State) ->
    try
	<<Mac:20/binary, " ",
	  Time:64, " ",
	  NodeString/binary>> = list_to_binary(Rest),
	case {mac([<<Time:64>>,NodeString]), abs(seconds() - Time)} of
	    {Mac, Delta} when Delta < 300 ->
		add_node(NodeString);
	    {Mac, Delta} ->
		discovery_log:debug(?MODULE, process_packet, ["expired packet", Delta]);
	    _ ->
		ok
	end
    catch
	error: {badmatch, _} ->
	    io:format("Bad match~n")
    end,
    State.

%% pings node to connect.
add_node(NodeDef) ->
    Node = list_to_atom(binary_to_list(NodeDef)),    
    net_adm:ping(Node).
