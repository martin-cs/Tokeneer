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
-- BioDeviceSim
--
-- Implementation Notes:
--    None.
--
------------------------------------------------------------------

with Ada.Strings.Fixed;
with BioAPI;
with MsgProc.Sim;

use type BioAPI.RateT;

package body BioDeviceSim
is

   Ack        : constant TcpIp.MessageT := TcpIp.NullMsg;
   RateNotSet : constant BioAPI.RateT   := -1;

   --------------------------------------------------------------------
   -- State
   --------------------------------------------------------------------

   Operational   : Boolean := True;
   NoImage       : Boolean := True;
   ImageTemplate : BioAPI.IDT := (others => ASCII.Nul);
   ImageLength   : Natural    := 0;
   FAR           : BioAPI.RateT := RateNotSet;
   FRR           : BioAPI.RateT := RateNotSet;

   --------------------------------------------------------------------
   --
   -- Local Subprograms
   --
   --------------------------------------------------------------------

   --------------------------------------------------------------------
   -- RateString
   --
   -- Description:
   --    Returns the given rate in string format.
   --
   -- Implementation Notes:
   --    None.
   --
   --------------------------------------------------------------------
   function RateString (Rate : BioAPI.RateT) return String
   is
   begin

      return Ada.Strings.Fixed.Trim
        (BioAPI.RateT'Image(Rate), Ada.Strings.Both);

   end RateString;


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
        "','noImage?':'" & Boolean'Image(NoImage) &
        "','image':{'imageTemplate':'" & ImageTemplate(1..ImageLength) &
        "','FAR':'" &  RateString (FAR) &
        "','FRR':'" &  RateString (FRR) &
        "'}}";

   begin

      Response.Length := ResponseText'Length;
      Response.Data(1 .. Response.Length) := ResponseText;
      TcpIp.Sim.Send ( IsAdmin => IsAdmin,
                       Client => Client,
                       Outgoing => Response);

   end SendResponse;


   --------------------------------------------------------------------
   -- SendImageReady
   --
   -- Description:
   --    Creates the image ready response using the package state, and
   --    sends it to the client.
   --
   -- Implementation Notes:
   --    None.
   --
   --------------------------------------------------------------------
   procedure SendImageReady (IsAdmin : in Boolean;
                             Client  : in TcpIp.Sim.ClientT)
   is

      Response     : TcpIp.MessageT := TcpIp.NullMsg;
      ResponseText : String :=
        "ready='" & Boolean'Image(not NoImage) & "'";

   begin

      Response.Length := ResponseText'Length;
      Response.Data(1 .. Response.Length) := ResponseText;
      TcpIp.Sim.Send ( IsAdmin => IsAdmin,
                       Client => Client,
                       Outgoing => Response);

   end SendImageReady;


   --------------------------------------------------------------------
   -- Verify
   --
   -- Description:
   --    Creates the verify response using package state and the given
   --    parameter, and sends it to the client.
   --
   -- Implementation Notes:
   --    None.
   --
   --------------------------------------------------------------------
   procedure Verify (Message : in TcpIp.MessageT;
                     IsAdmin : in Boolean;
                     Client  : in TcpIp.Sim.ClientT)
   is

      Matched        : Boolean;
      TemplateString : String := MsgProc.GetStringByPos(Message.Data, 1);
      FARString      : String := MsgProc.GetStringByPos(Message.Data, 2);
      TargetFAR      : BioAPI.RateT := BioAPI.RateT'Value (FARString);

   begin

      Matched :=
        TemplateString = ImageTemplate (1 .. TemplateString'Length) and
        TemplateString'Length = ImageLength and
        FAR <= TargetFAR;

      declare
         Response     : TcpIp.MessageT := TcpIp.NullMsg;
         ResponseText : String :=
           "Matched='" & Boolean'Image(Matched) &
           "',FARAchieved:'" & RateString (FAR) &
           "',BioReturn:'Ok'";
      begin
         Response.Length := ResponseText'Length;
         Response.Data(1 .. Response.Length) := ResponseText;
         TcpIp.Sim.Send ( IsAdmin => IsAdmin,
                          Client => Client,
                          Outgoing => Response);
      end;

   end Verify;


   --------------------------------------------------------------------
   -- SetImage
   --
   -- Description:
   --    Sets package image state according to values in message.
   --
   --
   -- Implementation Notes:
   --    None.
   --
   --------------------------------------------------------------------
   procedure SetImage (ImageDict : in MsgProc.DictionaryT)
   is

      ImageString : String := MsgProc.GetStringByKey
        (Dic => ImageDict, Key => "imageTemplate");

   begin

       ImageLength := ImageString'Length;

       ImageTemplate := (others => ASCII.Nul);
       ImageTemplate (1 .. ImageLength) := ImageString;

       FAR := BioAPI.RateT'Value(MsgProc.GetStringByKey
                                 (Dic => ImageDict, Key => "FAR"));

       FRR := BioAPI.RateT'Value(MsgProc.GetStringByKey
                                 (Dic => ImageDict, Key => "FRR"));

   end SetImage;


   --------------------------------------------------------------------
   -- SetState
   --
   -- Description:
   --    Sets package state according to values in message.
   --
   --
   -- Implementation Notes:
   --    None.
   --
   --------------------------------------------------------------------
   procedure SetState (Message : in TcpIp.MessageT)
   is

      StateDict : MsgProc.DictionaryT :=
        MsgProc.GetDictionary(Msg => Message.Data, Arg => 1);

      ImageDict : MsgProc.DictionaryT :=
        MsgProc.GetDictionaryByKey(Dic => StateDict, Key => "image");

   begin
       Operational := Boolean'Value
         (MsgProc.GetStringByKey(Dic => StateDict,
                                 Key => "operational?"));
       NoImage := Boolean'Value
         (MsgProc.GetStringByKey(Dic => StateDict,
                                 Key => "noImage?"));

       SetImage (ImageDict);

   end SetState;


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

      if MsgProc.Sim.MatchCommand ( "imageReady", Message) then

         SendImageReady (IsAdmin, Client);

      elsif MsgProc.Sim.MatchCommand ( "verify", Message) then

         Verify(Message, IsAdmin, Client);

      elsif MsgProc.Sim.MatchCommand ( "reset", Message) then

         Operational   := True;
         NoImage       := True;
         ImageTemplate := (others => ASCII.Nul);
         ImageLength   := 0;
         FAR           := RateNotSet;
         FRR           := RateNotSet;

         TcpIp.Sim.Send (IsAdmin, Client, Ack);

      elsif MsgProc.Sim.MatchCommand ( "supplyImage", Message) then

         declare
            ImageDict : MsgProc.DictionaryT :=
              MsgProc.GetDictionary(Msg => Message.Data, Arg => 1);
         begin
            SetImage (ImageDict);
            NoImage := False;
         end;

         TcpIp.Sim.Send (IsAdmin, Client, Ack);

      elsif MsgProc.Sim.MatchCommand ( "setState", Message) then

         SetState (Message);
         TcpIp.Sim.Send (IsAdmin, Client, Ack);

      elsif MsgProc.Sim.MatchCommand ( "getState", Message) then

         SendResponse (IsAdmin, Client);

      end if;


   end Handle;


end BioDeviceSim;
