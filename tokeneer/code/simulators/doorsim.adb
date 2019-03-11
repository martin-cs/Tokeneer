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
-- DoorSim
--
-- Implementation Notes:
--    None.
--
------------------------------------------------------------------

with MsgProc.Sim;

package body DoorSim
is

   Ack : constant TcpIp.MessageT := TcpIp.NullMsg;

   --------------------------------------------------------------------
   -- State
   --------------------------------------------------------------------

   Operational : Boolean := True;
   Closed      : Boolean := True;
   Locked      : Boolean := True;

   --------------------------------------------------------------------
   --
   -- Local Subprograms
   --
   --------------------------------------------------------------------

   --------------------------------------------------------------------
   -- SendResponse
   --
   -- Description:
   --    Creates the stock response using the package state, and
   --    sends it to the client.
   --
   -- Implementation Notes:
   --    None.
   --
   --------------------------------------------------------------------
   procedure SendResponse (IsAdmin : in Boolean;
                           Client  : in TcpIp.Sim.ClientT)
   is

      Response     : TcpIp.MessageT := TcpIp.NullMsg;
      ResponseText : String :=
        "{'operational?':'" & Boolean'Image(Operational) &
        "','locked?':'" & Boolean'Image(Locked) &
        "','closed?':'" & Boolean'Image(Closed) & "'}";

   begin

      Response.Length := ResponseText'Length;
      Response.Data(1 .. Response.Length) := ResponseText;
      TcpIp.Sim.Send ( IsAdmin => IsAdmin,
                       Client => Client,
                       Outgoing => Response);

   end SendResponse;


   --------------------------------------------------------------------
   --
   -- Exported Subprograms
   --
   --------------------------------------------------------------------

   --------------------------------------------------------------------
   -- Handle
   --
   -- Implementation Notes:
   --    None.
   --
   --------------------------------------------------------------------
   procedure Handle (Message : in TcpIp.MessageT;
                     IsAdmin : in Boolean;
                     Client  : in TcpIp.Sim.ClientT)
   is
   begin

      if MsgProc.Sim.MatchCommand ( "breakOpen", Message) then

         -- How is "breakOpen" different to "open"?
         Closed := False;
         TcpIp.Sim.Send (IsAdmin, Client, Ack);

      elsif MsgProc.Sim.MatchCommand ( "open", Message) then

         Closed := False;
         TcpIp.Sim.Send (IsAdmin, Client, Ack);

      elsif MsgProc.Sim.MatchCommand ( "close", Message) then

         Closed := True;
         TcpIp.Sim.Send (IsAdmin, Client, Ack);

      elsif MsgProc.Sim.MatchCommand ( "lock", Message) then

         Locked := True;
         SendResponse (IsAdmin, Client);

      elsif MsgProc.Sim.MatchCommand ( "unlock", Message) then

         Locked := False;
         SendResponse (IsAdmin, Client);

      elsif MsgProc.Sim.MatchCommand ( "setState", Message) then

         declare
            StateDict : MsgProc.DictionaryT :=
              MsgProc.GetDictionary(Msg => Message.Data, Arg => 1);
         begin
            Operational := Boolean'Value
              (MsgProc.GetStringByKey(Dic => StateDict,
                                      Key => "operational?"));
            Closed := Boolean'Value
              (MsgProc.GetStringByKey(Dic => StateDict,
                                      Key => "closed?"));
            Locked := Boolean'Value
              (MsgProc.GetStringByKey(Dic => StateDict,
                                      Key => "locked?"));
         end;
         TcpIp.Sim.Send (IsAdmin, Client, Ack);

      elsif MsgProc.Sim.MatchCommand ( "getState", Message) then

         SendResponse (IsAdmin, Client);

      end if;


   end Handle;


end DoorSim;

