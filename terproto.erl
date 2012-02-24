-module( terproto ).
-compile( export_all ).
-import( terpacket ).
-define( HOST, "192.168.1.114" ).
-define( PORT, 7777 ).
-define( RECV_TIMEOUT, 5000 ).

start() ->
   Sock = connect(),
   ProtoPid = spawn_link( ?MODULE, terproto_loop, [ self(), Sock, <<>> ] ),
   register( ?MODULE, ProtoPid ),
   ok = gen_tcp:controlling_process( Sock, ProtoPid ).



connect() ->
   HostName = ?HOST,
   case gen_tcp:connect( HostName, ?PORT, 
         [binary, {packet, 0}, {send_timeout, 5000}], 5000) of
      {ok, Sock} ->
         Sock;
      {error, Reason} ->
         io:format( "Connection Error: ~p~n", [Reason] ),
         connect()
   end.

send( TerPacket = { terpacket, _, _} ) ->
   ?MODULE ! TerPacket.

recv( Type ) ->
   receive
      {terpacket, Type, _} = Terpacket ->
         Terpacket
   after ?RECV_TIMEOUT ->
         {error, timout}
   end.

terproto_loop( Parent, Sock, Buffer ) ->
   {Length, ActualLength, Payload} = if
      byte_size(Buffer) > 4 ->
         <<Length1:32/little-signed, Data2/binary>> = Buffer,
         {Length1, byte_size(Data2), Data2 };
      true ->
         {1,0, <<>> }
   end,
   NextBuffer = if 
      ActualLength > Length ->
         Parent ! raw_packet_to_terpacket( binary:part( Payload, 0, Length ) ),
         binary:part( Payload, Length, ActualLength - Length );
      ActualLength == Length ->
         Parent ! raw_packet_to_terpacket( Payload ),
         <<>>;
      ActualLength < Length ->
         receive
            {tcp, Sock, Data} ->
               <<Buffer/binary, Data/binary>>;
            {terpacket, _Operation, _Payload } = Terpacket ->
               ok = gen_tcp:send( Sock, terpacket_to_raw_packet( Terpacket ) ),
               Buffer
         end
   end,
   terproto_loop( Parent, Sock, NextBuffer ).

raw_packet_to_terpacket( Data ) ->
   <<Operation, Payload/binary>> = Data,
   {terpacket, Operation, Payload}.

terpacket_to_raw_packet( {terpacket, Operation, Payload} ) ->
   craft_packet( Operation, [Payload] ).

craft_packet( Type, IOList ) when is_number(Type) and is_list(IOList) ->
   PayloadLength = iolist_size(IOList) + 1,
   [<<PayloadLength:32/little-signed, Type:8>>, IOList].
