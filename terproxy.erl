-module( terproxy ).
-import( terbot ).
-import( terpacket ).
-include( "terpacket.hrl" ).
-compile( export_all ).

start() ->
   register( mitm_pid, self() ),
   ClientPid = spawn_link( terproxy, client_side, [] ),
   ServerPid = spawn_link( terproxy, server_side, [] ),
   mitm( ClientPid, ServerPid ).


client_side() ->
   {ok, LSock} = gen_tcp:listen(9999, [binary, {packet, 0}, {reuseaddr, true}]),
   {ok, ProxySock} = gen_tcp:accept( LSock ),
   io:format( "Client connected!~n" ),
   ProtoPid = spawn_link( terbot, protocol_func, [ self(), ProxySock, <<>> ] ),
   gen_tcp:controlling_process( ProxySock, ProtoPid ),
   listen_loop( client_side, ProtoPid ).

server_side() ->
   {ok, ClientSock} = gen_tcp:connect( "192.168.1.109", 7777, [binary, {packet, 0} ] ),
   io:format( "Connected to server!~n" ),
   ProtoPid = spawn_link( terbot, protocol_func, [ self(), ClientSock, <<>> ] ),
   gen_tcp:controlling_process( ClientSock, ProtoPid ),
   listen_loop( server_side, ProtoPid).

listen_loop( Side, ProtoPid ) ->
   receive
      {terpacket, _Operation, _Payload} = A ->
         mitm_pid ! { Side, A };
      {from_mitm, B } ->
         ProtoPid ! B
   end,
   listen_loop( Side, ProtoPid ).

mitm( ClientPid, ServerPid ) ->
   receive
      { client_side, {terpacket, _Operation, _Payload} = TerPacket } ->
         display_terrecord( terpacket:terpacket_to_record( TerPacket ) ),
         ServerPid ! {from_mitm, TerPacket};
      { server_side, {terpacket, _Operation, _Payload} = TerPacket } ->
         ClientPid ! {from_mitm, TerPacket}
   end,
   mitm( ClientPid, ServerPid ).

display_terrecord( Data = #tp_update_player{} ) ->
   io:format( "<~p~n", [Data] );

display_terrecord( _ ) ->
   true.
