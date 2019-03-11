------------------------------------------------------------------------------
-- Tokeneer ID Station - Peripheral Simulator
--
-- Copyright (c) 2008, Praxis High Integrity Systems Limited
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
--  * Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--
--  * Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in
--    the documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
-- PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
-- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
-- EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
-- PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
------------------------------------------------------------------------------


------------------------------------------------------------------
-- TcpIp.Sim
--
-- Implementation Notes:
--    TCP/IP sockets to connect to are hard-coded in this body.
--
------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Strings.Fixed;
with GNAT.Sockets;
with Ada.Command_Line;

use type GNAT.Sockets.Selector_Status;

package body TcpIp.Sim is

   --
   -- Current SPRE information...
   --
   PortalPort  : GNAT.Sockets.Port_Type := 12001;
   AdminPort   : GNAT.Sockets.Port_Type := 12000;

   type SPREMachineT is
   record
     Data : String(1 .. 30);
     Length : Natural range 0 .. 30;
   end record;

   SPREMachine : SPREMachineT :=
     (Data => (others => ASCII.nul),
      Length => 0);

   -- Enumeration of the currently available servers
   type ServerT is (Admin,Portal);

   --
   -- PortTo:
   --
   --    Used internally to keep track of the state of
   --    each of the available SPRE device driver server ports.
   --

   type PortStateT is (Connected, NotConnected);

   -- Are we connected to the port? if so, which socket and channel?
   type PortInfoT is record
      State   : PortStateT;
      Socket  : GNAT.Sockets.Socket_Type;
      Channel : GNAT.Sockets.Stream_Access;
   end record;

   type ClientPortInfoT is array (ClientT) of PortInfoT;

   type ConnectStateT is array (ServerT) of ClientPortInfoT;
   PortTo : ConnectStateT := ( others  => ( others =>
                                 ( State   => NotConnected,
                                   Socket  => GNAT.Sockets.No_Socket,
                                   Channel => GNAT.Sockets.Stream(
                                                  GNAT.Sockets.No_Socket) )));

   -- Has the socket library been initialized?
   type WinSockStateT is (Uninitialized, Initialized);
   WinSockState : WinSockStateT := Uninitialized;


   -- Once a socket is open, can use GNAT.Sockets.Check_Selector to
   -- monitor changes in state of a set of sockets. ReadSet and WriteSet
   -- represent the two sets of sockets that will be maintained.

   ReadSet      : GNAT.Sockets.Socket_Set_Type;
   WriteSet     : GNAT.Sockets.Socket_Set_Type;
   CheckTimeout : constant Duration := 5.0;
   Selector     : GNAT.Sockets.Selector_Type;
   Queue_Length : Positive := 15;

   --------------------------------------------------------------------
   --
   -- Local Subprograms
   --
   --------------------------------------------------------------------

   --------------------------------------------------------------------
   -- StartDebug
   --
   -- Description:
   --   Starts a TCPIP log file.
   --
   -- Implementation Notes:
   --    Set to Null in delivered system.
   --
   --------------------------------------------------------------------
   procedure StartDebug
   is
            MyError : Ada.Text_IO.File_Type;

   begin
      null;

            Ada.Text_IO.Create(File => MyError,
                               Mode => Ada.Text_IO.Out_File,
                               Name => "TCPIP.Sim.Log");
            Ada.Text_Io.Close(MyError);
   end StartDebug;


   --------------------------------------------------------------------
   -- DebugOutput
   --
   -- Description:
   --   Writes a debug message to the TCPIP log file.
   --
   -- Implementation Notes:
   --    Set to Null in delivered system.
   --
   --------------------------------------------------------------------
   procedure DebugOutput(Text : String)
   is
            MyError : Ada.Text_IO.File_Type;

   begin

      null;
               Ada.Text_IO.Open(File => MyError,
                                Mode => Ada.Text_IO.Append_File,
                                Name => "TCPIP.Sim.Log");
               Ada.Text_IO.Put_Line(File => MyError,
                                    Item => Text);
               Ada.Text_IO.Close(MyError);
   end DebugOutput;

   --------------------------------------------------------------------
   -- MsgToRead
   --
   -- Description:
   --    Checks whether there is a message waiting to be read from a socket.
   --
   -- Implementation Notes:
   --    Adds socket to ReadSet, so shouldn't really be a function...
   --
   --------------------------------------------------------------------

   procedure MsgToRead ( Server :    out ServerT;
                         Client :    out ClientT;
                         ToRead :    out Boolean)
   is

      CheckStatus  : GNAT.Sockets.Selector_Status;

   begin

      Server := ServerT'First;
      Client := ClientT'First;
      ToRead := False;
      GNAT.Sockets.Empty(ReadSet);

      for Clnt in ClientT loop
         for Srv in ServerT loop

            -- Add sockets to the ReadSet
            if PortTo(Srv)(Clnt).State = Connected then

               GNAT.Sockets.Set ( Item   => ReadSet,
                                  Socket => PortTo(Srv)(Clnt).Socket );
            end if;

         end loop;
      end loop;

      -- Check for any messages
      GNAT.Sockets.Check_Selector ( Selector     => Selector,
                                    R_Socket_Set => ReadSet,
                                    W_Socket_Set => WriteSet,
                                    Status       => CheckStatus,
                                    Timeout      => CheckTimeout );

      if CheckStatus = GNAT.Sockets.Completed then

      for Clnt in ClientT loop
         for Srv in ServerT loop

            if GNAT.Sockets.Is_Set
              ( Item   => ReadSet,
                Socket => PortTo(Srv)(Clnt).Socket ) then

               ToRead := True;
               Client := Clnt;
               Server := Srv;

            end if;

         end loop;
      end loop;

      -- For debugging...
      else
         DebugOutput("Data not available to read");
      end if;

   end MsgToRead;


   --------------------------------------------------------------------
   -- ReadMsg
   --
   -- Description:
   --    Reads message from Server, terminating on reading the message
   --    delineation sequence (CR,LF). The Length field does not include
   --    the delineation sequence i.e. only length of actual message data.
   --
   -- Implementation Notes:
   --    There may well be more data to read. This will be 'cleared' when
   --    we next attempt to SendMsg.
   --
   --------------------------------------------------------------------

   procedure ReadMsg ( Server  : in     ServerT;
                       Client  : in     ClientT;
                       Msg     :    out MessageT;
                       Success :    out Boolean ) is

   begin

      Success := True;

      for i in MessageIndexT'Range loop

         Character'Read ( PortTo(Server)(Client).Channel,
                          Msg.Data(i) );

         if Msg.Data(i)     = ASCII.Lf and then
            Msg.Data(i - 1) = ASCII.Cr then

            Msg.Data(i - 1 .. i) := (others => ASCII.Nul);
            Msg.Length := i - 2;
            exit;

         end if;

      end loop;

      -- For debugging...
      DebugOutput("Rcvd: " & Msg.Data(1..Msg.Length));


   exception
      when E : others =>
         Success := False;
         DebugOutput("Read Error -- dropping socket");
         PortTo(Server)(Client).State := NotConnected;
         PortTo(Server)(Client).Socket := GNAT.Sockets.No_Socket;

   end ReadMsg;


   --------------------------------------------------------------------
   -- WriteLine
   --
   -- Description:
   --    Writes a string to the given channel.
   --
   -- Implementation Notes:
   --    None.
   --
   --------------------------------------------------------------------

   procedure WriteLine (Channel : in GNAT.Sockets.Stream_Access;
                        Message : in String )
   is
   begin

      Character'Write ( Channel, ''' );
      Character'Write ( Channel, 'O' );
      Character'Write ( Channel, 'K' );
      Character'Write ( Channel, ''' );

      for i in Message'Range loop

         Character'Write ( Channel, Message(i) );

      end loop;

      Character'Write ( Channel, ASCII.Cr );
      Character'Write ( Channel, ASCII.Lf );

   end WriteLine;


   --------------------------------------------------------------------
   -- SendMsg
   --
   -- Description:
   --    Sends Msg.Data to Channel.
   --
   -- Implementation Notes:
   --    Check status of Socket before sending data. If the Socket has data
   --    waiting to be read, Success is set to false, and the channel is read
   --    until an exception is raised (to clear the channel).
   --
   --------------------------------------------------------------------

   procedure SendMsg ( Server  : in     ServerT;
                       Client  : in     ClientT;
                       Msg     : in     MessageT;
                       Success :    out Boolean )
   is
      CheckStatus  : GNAT.Sockets.Selector_Status;
   begin

      Success := False;

      GNAT.Sockets.Set ( Item   => WriteSet,
                         Socket => PortTo(Server)(Client).Socket );

      GNAT.Sockets.Check_Selector ( Selector     => Selector,
                                    R_Socket_Set => ReadSet,
                                    W_Socket_Set => WriteSet,
                                    Status       => CheckStatus,
                                    Timeout      => 0.01 );

      if GNAT.Sockets.Is_Set ( Item   => ReadSet,
                               Socket => PortTo(Server)(Client).Socket ) then

         GNAT.Sockets.Clear ( Item   => ReadSet,
                              Socket => PortTo(Server)(Client).Socket );

         declare
            DummyChar : Character;
         begin
            for i in MessageIndexT'Range loop
               Character'Read ( PortTo(Server)(Client).Channel,
                                DummyChar );
            end loop;

         exception
            when E : others =>
               DebugOutput("Socket had readable data.");

         end;

      elsif GNAT.Sockets.Is_Set
        ( Item   => WriteSet,
          Socket => PortTo(Server)(Client).Socket ) then

         GNAT.Sockets.Clear ( Item   => WriteSet,
                              Socket => PortTo(Server)(Client).Socket );

         Success := True;

         declare
            Message : String (1 .. Msg.Length) := Msg.Data(1 .. Msg.Length);
         begin
            WriteLine (PortTo(Server)(Client).Channel, Message);
         end;

      end if;

      -- For debugging...
      DebugOutput("Sent: " & Msg.Data(1..Msg.Length));


   exception
      when E : others =>
         DebugOutput("Send Error.");

   end SendMsg;

   --------------------------------------------------------------------
   -- ConnectToClient
   --
   -- Description:
   --    Waits for the specified client to connect to the waiting socket.
   --
   -- Implementation Notes:
   --    Initializes the socket library.
   --
   --------------------------------------------------------------------

   procedure ConnectToClient ( IsAdmin : in     Boolean;
                               Client  : in     ClientT) is

      Address      : GNAT.Sockets.Sock_Addr_Type := GNAT.Sockets.No_Sock_Addr;
      Server       : ServerT;
      Temp_Socket  : GNAT.Sockets.Socket_Type;
      Temp_Socket_Connected  : Boolean := False;

   begin

      if WinSockState = Initialized then

         -- Set up socket address and Server into PortTo
         Address.Addr := GNAT.Sockets.Addresses
           (E => GNAT.Sockets.Get_Host_By_Name
                 (Name => GNAT.Sockets.Host_Name ),
            N => 1);

         if IsAdmin then
            Address.Port := AdminPort;
            Server       := Admin;
         else
            Address.Port := PortalPort;
            Server       := Portal;
         end if;


         -- If we are already connected, then do nothing.
         if PortTo(Server)(Client).State = NotConnected then

            GNAT.Sockets.Create_Socket ( Socket => Temp_Socket );
            Temp_Socket_Connected := True;

            GNAT.Sockets.Set_Socket_Option (
                Socket => Temp_Socket,
                Level  => GNAT.Sockets.Socket_Level,
                Option => ( Name    => GNAT.Sockets.Reuse_Address,
                            Enabled => True ));

            GNAT.Sockets.Bind_Socket ( Socket => Temp_Socket,
                                       Address => Address );
            GNAT.Sockets.Listen_Socket ( Socket => Temp_Socket,
                                         Length => Queue_Length );

            GNAT.Sockets.Accept_Socket
              (Server  => Temp_Socket,
               Socket  => PortTo(Server)(Client).Socket,
               Address => Address);

            GNAT.Sockets.Close_Socket (Temp_Socket);
            Temp_Socket_Connected := False;

            PortTo(Server)(Client).Channel :=
              GNAT.Sockets.Stream (PortTo(Server)(Client).Socket);

            PortTo(Server)(Client).State := Connected;

            -- TIS & TestTIS expect a response when they connect to us
            WriteLine (PortTo(Server)(Client).Channel, "Hello ducky");

         end if;

      end if;

   exception

      when E : others =>

         if IsAdmin then
            DebugOutput(   "Error listening on server... "
                           & SPREMachine.Data(1 .. SPREMachine.Length)
                           & " Port"
                           & GNAT.Sockets.Port_Type'Image(AdminPort));
         else
            DebugOutput(   "Error listening on server... "
                           & SPREMachine.Data(1 .. SPREMachine.Length)
                           & " Port"
                           & GNAT.Sockets.Port_Type'Image(PortalPort));
         end if;

         if Temp_Socket_Connected then
            GNAT.Sockets.Close_Socket (Temp_Socket);
         end if;

         if PortTo(Server)(Client).State = Connected then
            GNAT.Sockets.Close_Socket (PortTo(Server)(Client).Socket);
         end if;

   end ConnectToClient;


   --------------------------------------------------------------------
   -- DisconnectFromClient
   --
   -- Description:
   --    Disconects the specified socket.
   --
   -- Implementation Notes:
   --    Finalizes the socket library if both ports are disconnected.
   --
   --------------------------------------------------------------------

   procedure DisconnectFromClient (IsAdmin : in     Boolean;
                                   Client  : in     ClientT;
                                   Success :    out Boolean) is

      Address      : GNAT.Sockets.Sock_Addr_Type := GNAT.Sockets.No_Sock_Addr;
      Server       : ServerT;
      AllClosed    : Boolean;

   begin

      if WinSockState = Initialized then
         -- Set up socket address and Server into PortTo
         Address.Addr := GNAT.Sockets.Addresses
           (GNAT.Sockets.Get_Host_By_Name
                (SPREMachine.Data(1 .. SPREMachine.Length)),
            1);

         if IsAdmin then
            Address.Port := AdminPort;
            Server       := Admin;
         else
            Address.Port := PortalPort;
            Server       := Portal;
         end if;

         -- If we are not connected, then do nothing.
         if PortTo(Server)(Client).State = Connected then

            GNAT.Sockets.Close_Socket ( PortTo(Server)(Client).Socket );

            PortTo(Server)(Client).Socket  := GNAT.Sockets.No_Socket;
            PortTo(Server)(Client).Channel :=
              GNAT.Sockets.Stream (GNAT.Sockets.No_Socket);
            PortTo(Server)(Client).State   := NotConnected;

         end if;

         -- If all ports are closed, finalize socket library

         AllClosed := True;
         for Clnt in ClientT loop
            for Srv in ServerT loop

               if PortTo(Srv)(Clnt).State = Connected then
                  AllClosed := False;
               end if;
               exit when not AllClosed;
            end loop;
            exit when not AllClosed;
         end loop;

         if AllClosed then
            GNAT.Sockets.Close_Selector ( Selector => Selector );
            GNAT.Sockets.Finalize;
            WinSockState := Uninitialized;
         end if;

      end if;

      Success := True;

   exception

      when E : others =>

         if IsAdmin then
            DebugOutput(   "Error disconnecting server... "
                           & SPREMachine.Data(1 .. SPREMachine.Length)
                           & " Port"
                           & GNAT.Sockets.Port_Type'Image(AdminPort)
                           & " Client"
                           & ClientT'Image(Client));
         else
            DebugOutput(   "Error disconnecting server... "
                           & SPREMachine.Data(1 .. SPREMachine.Length)
                           & " Port"
                           & GNAT.Sockets.Port_Type'Image(PortalPort)
                           & " Client"
                           & ClientT'Image(Client));
         end if;

         if PortTo(Server)(Client).State = Connected then
            GNAT.Sockets.Close_Socket (PortTo(Server)(Client).Socket);
         end if;
         Success := False;

   end DisconnectFromClient;


   --------------------------------------------------------------------
   -- SendOK
   --
   -- Description:
   --    Sends a message to the specified socket.
   --    Reports success or failure.
   --
   -- Implementation Notes:
   --    Might need to make this routine public.
   --
   --------------------------------------------------------------------

   procedure SendOK ( IsAdmin  : in     Boolean;
                      Client   : in     ClientT;
                      Outgoing : in     MessageT;
                      Success  :    out Boolean)
   is

      Server       : ServerT;
      SendOK       : Boolean := False;

   begin

      if WinSockState = Initialized then

         if IsAdmin then
            Server := Admin;
         else
            Server := Portal;
         end if;

         -- Communicate with the server
         if PortTo(Server)(Client).State = Connected then

            SendMsg  ( Server  => Server,
                       Client  => Client,
                       Msg     => Outgoing,
                       Success => SendOK );

         end if;

      end if;

      Success := SendOK;

   exception

      when E : others =>

         DebugOutput( "Error sending with server. " );
         Success := False;

   end SendOK;


   --------------------------------------------------------------------
   --
   -- Local Tasks
   --
   --------------------------------------------------------------------

   --------------------------------------------------------------------
   -- MonitorAdminConnections
   --
   -- Description:
   --    Listens for connections on the admin port
   --
   -- Implementation Notes:
   --    ConnectToClient blocks until a connection is made, so the
   --    only way to terminate this task is with an 'abort'.
   --
   --------------------------------------------------------------------
   task MonitorAdminConnections is
      entry Start;
   end MonitorAdminConnections;

   task body MonitorAdminConnections is
      Client : ClientT;
   begin
      accept Start;

      Client := ClientT'First;
      loop

         -- Connect to next client
         ConnectToClient (IsAdmin => True,
                          Client  => Client);

         if Client < ClientT'Last then
            Client := ClientT'Succ(Client);
         else
            Client := ClientT'First;
         end if;

      end loop;

   end MonitorAdminConnections;


   --------------------------------------------------------------------
   -- MonitorNonAdminConnections
   --
   -- Description:
   --    Listens for connections on the portal port
   --
   -- Implementation Notes:
   --    ConnectToClient blocks until a connection is made, so the
   --    only way to terminate this task is with an 'abort'.
   --
   --------------------------------------------------------------------
   task MonitorNonAdminConnections is
      entry Start;
   end MonitorNonAdminConnections;

   task body MonitorNonAdminConnections is
      Client : ClientT;
   begin
      accept Start;

      Client := ClientT'First;
      loop

         -- Connect to next client
         ConnectToClient (IsAdmin => False,
                          Client  => Client);

         if Client < ClientT'Last then
            Client := ClientT'Succ(Client);
         else
            Client := ClientT'First;
         end if;

      end loop;

   end MonitorNonAdminConnections;


   --------------------------------------------------------------------
   --
   -- Exported Subprograms
   --
   --------------------------------------------------------------------

   --------------------------------------------------------------------
   -- OpenAll
   --
   -- Implementation Notes:
   --    None.
   --------------------------------------------------------------------
   procedure OpenAll
   is
   begin
      StartDebug;
      MonitorAdminConnections.Start;
      MonitorNonAdminConnections.Start;
   end OpenAll;


   --------------------------------------------------------------------
   -- CloseAll
   --
   -- Implementation Notes:
   --    Suppress success flags - we will force closure by shutting down.
   --
   --------------------------------------------------------------------
   procedure CloseAll
   is
      Ignored : Boolean;
   begin

      abort MonitorAdminConnections;
      abort MonitorNonAdminConnections;

      for Client in ClientT loop

         DisconnectFromClient (IsAdmin => False,
                               Client  => Client,
                               Success => Ignored);
         DisconnectFromClient (IsAdmin => True,
                               Client  => Client,
                               Success => Ignored);
      end loop;

   end CloseAll;


   --------------------------------------------------------------------
   -- Send
   --
   -- Implementation Notes:
   --    None.
   --
   --------------------------------------------------------------------

   procedure Send ( IsAdmin  : in     Boolean;
                    Client   : in     ClientT;
                    Outgoing : in     MessageT)
   is
      DontCare : Boolean;
   begin

      SendOK (IsAdmin, Client, Outgoing, DontCare);

   end Send;


   --------------------------------------------------------------------
   -- Receive
   --
   -- Implementation Notes:
   --    None.
   --
   --------------------------------------------------------------------

   procedure Receive ( IsAdmin  :    out Boolean;
                       Client   :    out ClientT;
                       Incoming :    out MessageT;
                       Success  :    out Boolean)
   is

      Server       : ServerT := ServerT'First;
      ReadOK       : Boolean := False;

   begin

      Incoming := NullMsg;
      Client   := ClientT'First;

      if WinSockState = Initialized then

         MsgToRead( Server => Server,
                    Client => Client,
                    ToRead => ReadOK);

         if ReadOK then

            ReadMsg ( Server  => Server,
                      Client  => Client,
                      Msg     => Incoming,
                      Success => ReadOK );

         end if;


      end if;

      Success := ReadOK;
      IsAdmin := Server = Admin;

   exception

      when E : others =>

         DebugOutput( "Error receiving with server. " );
         Success := False;

   end Receive;


   --------------------------------------------------------------------
   -- Init
   --
   -- Implementation Notes:
   --    None.
   --
   --------------------------------------------------------------------
   procedure Init (Success  :    out Boolean)
   is

      InvalidArgument : exception;

      procedure PrintHelp is
      begin
         Ada.Text_IO.Put_Line("Optional arguments are:");
         Ada.Text_IO.Put_Line("[<Workstation port> [<Portal #1 port>]]");
         Ada.Text_IO.Put_Line("where <Workstation port> is a numeric");
         Ada.Text_IO.Put_Line("                           - default value: " &
                              GNAT.Sockets.Port_Type'Image(AdminPort));
         Ada.Text_IO.Put_Line("      <Portal #1 port>   is a numeric ");
         Ada.Text_IO.Put_Line("                           - default value: " &
                              GNAT.Sockets.Port_Type'Image(PortalPort));

      end PrintHelp;

   begin

      Success := True;

      -- First ensure that socket library is available.
      if WinSockState = Uninitialized then

         GNAT.Sockets.Initialize;
         WinSockState := Initialized;

         GNAT.Sockets.Create_Selector ( Selector => Selector );
      end if;


      SPREMachine.Length := GNAT.Sockets.Host_Name'Last;
      SPREMachine.Data(1 .. SPREMachine.Length) := GNAT.Sockets.Host_Name;

      if Ada.Command_Line.Argument_Count <= 2 then
         if Ada.Command_Line.Argument_Count >= 1 then
            if Ada.Command_Line.Argument(1) = "help" then
               PrintHelp;
               Success := False;
            else
               AdminPort :=
                 GNAT.Sockets.Port_Type'Value(Ada.Command_Line.Argument(1));
               if Ada.Command_Line.Argument_Count >= 2 then
                  PortalPort :=
                    GNAT.Sockets.Port_Type'Value(Ada.Command_Line.Argument(2));
               end if;
            end if;
         end if;

      else
         Success := False;
         Ada.Text_IO.Put_Line
           ("Wrong number of Arguments - optional arguments are:");
         Ada.Text_IO.Put_Line
           ("  [<Workstation port> [<Portal #1 port>]]");
         Ada.Text_IO.Put_Line
           ("  supply single argument 'help' for more information");

      end if;

   exception

      when E : others =>

         Success := False;
         Ada.Text_IO.Put_Line
           ("Invalid Arguments - optional arguments are:");
         Ada.Text_IO.Put_Line
           ("  [<Workstation port> [<Portal #1 port>]]");
         Ada.Text_IO.Put_Line
           ("  supply single argument 'help' for more information");

   end Init;

end TcpIp.Sim;


