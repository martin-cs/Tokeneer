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
-- DisplaySim
--
-- Implementation Notes:
--    None.
--
------------------------------------------------------------------

with Ada.Strings.Fixed;
with Ada.Text_IO;
with MsgProc.Sim;

package body DisplaySim
is

   Ack   : constant TcpIp.MessageT := TcpIp.NullMsg;
   AckOK : TcpIp.MessageT := TcpIp.NullMsg;

   AckOKString : constant String := ",Written:'True'";

   -- Arbitrary maximum length of a string
   MaxStringLength : constant := 1000;
   subtype IndexT is Natural range 0 .. MaxStringLength;
   subtype StringT is String (1 .. IndexT'Last);

   -- Arbitrary default length for the display
   DefaultLen : IndexT := 80;

   --------------------------------------------------------------------
   -- State
   --------------------------------------------------------------------

   Operational  : Boolean := True;

   Top          : StringT := (others => ASCII.Nul);
   TopLength    : IndexT  := 1;
   Scroll       : Boolean := False;

   Bottom       : StringT := (others => ASCII.Nul);
   BottomLength : IndexT  := 1;

   Len          : IndexT  := DefaultLen;

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

      TopString    : String := Top (1 .. TopLength);
      BottomString : String := Bottom (1 .. BottomLength);

      Response     : TcpIp.MessageT := TcpIp.NullMsg;
      ResponseText : String :=
        "{'top': {'text': '" & TopString &
        "', 'len': '" & Ada.Strings.Fixed.Trim(IndexT'Image(Len),
                                             Ada.Strings.Both) &
        "', 'scroll?': '" & Boolean'Image(Scroll) &
        "'}, 'bottom': {'text': '" & BottomString &
        "', 'len': '" & Ada.Strings.Fixed.Trim(IndexT'Image(Len),
                                             Ada.Strings.Both) &
        "'}}";

   begin

      Response.Length := ResponseText'Length;
      Response.Data(1 .. Response.Length) := ResponseText;
      TcpIp.Sim.Send ( IsAdmin => IsAdmin,
                       Client => Client,
                       Outgoing => Response);

   end SendResponse;


   --------------------------------------------------------------------
   -- SendMaxTextSize
   --
   -- Description:
   --    Creates a message with the maximum text size and
   --    sends it to the client.
   --
   -- Implementation Notes:
   --    None.
   --
   --------------------------------------------------------------------
   procedure SendMaxTextSize (IsAdmin : in Boolean;
                              Client  : in TcpIp.Sim.ClientT)
   is

      Response     : TcpIp.MessageT := TcpIp.NullMsg;
      ResponseText : String :=
        "Return:'" & Ada.Strings.Fixed.Trim(IndexT'Image(Len),
                                            Ada.Strings.Both)
        & "'";

   begin

      Response.Length := ResponseText'Length;
      Response.Data(1 .. Response.Length) := ResponseText;
      TcpIp.Sim.Send ( IsAdmin => IsAdmin,
                       Client => Client,
                       Outgoing => Response);

   end SendMaxTextSize;


   --------------------------------------------------------------------
   -- SetText
   --
   -- Description:
   --    Creates a message with the maximum text size and
   --    sends it to the client.
   --
   -- Implementation Notes:
   --    None.
   --
   --------------------------------------------------------------------
   procedure SetText (Text     :    out String;
                      Length   :    out IndexT;
                      Message  : in     TcpIp.MessageT)
   is

      TextString : String := MsgProc.GetStringByPos(Msg => Message.Data,
                                                    Arg => 1);

   begin

      Text := (others => ASCII.Nul);
      Text (1 .. TextString'Length) := TextString;
      Length := TextString'Length;

   end SetText;


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

      if MsgProc.Sim.MatchCommand ( "reset", Message) then

         Operational  := True;

         Top          := (others => ASCII.Nul);
         TopLength    := 1;
         Scroll       := False;

         Bottom       := (others => ASCII.Nul);
         BottomLength := 1;

         TcpIp.Sim.Send (IsAdmin, Client, Ack);

      elsif MsgProc.Sim.MatchCommand ( "getMaxTextSizeTop", Message) then

         SendMaxTextSize (IsAdmin, Client);

      elsif MsgProc.Sim.MatchCommand ( "getMaxTextSizeBottom", Message) then

         SendMaxTextSize (IsAdmin, Client);

      elsif MsgProc.Sim.MatchCommand ( "setTopText", Message) then

         SetText (Top, TopLength, Message);
         Scroll := False;
         TcpIp.Sim.Send (IsAdmin, Client, AckOK);

      elsif MsgProc.Sim.MatchCommand ( "setBottomText", Message) then

         SetText (Bottom, BottomLength, Message);
         TcpIp.Sim.Send (IsAdmin, Client, AckOK);

      elsif MsgProc.Sim.MatchCommand ( "setTopTextScrollable", Message) then

         SetText (Top, TopLength, Message);
         Scroll := True;
         TcpIp.Sim.Send (IsAdmin, Client, AckOK);

      elsif MsgProc.Sim.MatchCommand ( "setState", Message) then

         declare
            StateDict : MsgProc.DictionaryT :=
              MsgProc.GetDictionary(Msg => Message.Data, Arg => 1);
         begin
            Operational := Boolean'Value
              (MsgProc.GetStringByKey(Dic => StateDict,
                                      Key => "operational?"));
            Len := IndexT'Value
              (MsgProc.GetStringByKey(Dic => StateDict,
                                      Key => "len"));
         end;
         TcpIp.Sim.Send (IsAdmin, Client, Ack);

      elsif MsgProc.Sim.MatchCommand ( "read", Message) then

         SendResponse (IsAdmin, Client);

      end if;


   end Handle;

begin

   -- Set up sucessful ack message
   AckOK.Data(1 .. AckOKString'Length) := AckOKString;
   AckOK.Length := AckOKString'Length;

end DisplaySim;
