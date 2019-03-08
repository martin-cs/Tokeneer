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
-- TokenReaderSim
--
-- Implementation Notes:
--    None.
--
------------------------------------------------------------------

with Ada.Strings.Fixed;
with CardSim;
with CommonTypes;
with BasicTypes;
with MsgProc.Sim;
with TokenAPI;
with CertProc;

package body TokenReaderSim
is

   Ack : constant TcpIp.MessageT := TcpIp.NullMsg;


   --------------------------------------------------------------------
   -- State
   --------------------------------------------------------------------

   type ReaderT is record
      Name   : CommonTypes.String8T;
      Valid  : Boolean;
      RState : TokenAPI.ReaderStateT;
      CState : TokenAPI.CardStateT;
      Card   : CardSim.CardNameT;
   end record;

   type ReadersT is array (CommonTypes.String8ArrayI) of ReaderT;

   Readers : ReadersT := (others => (Name   => (others => ' '),
                                     Valid  => False,
                                     RState => TokenAPI.Unaware,
                                     CState => TokenAPI.Absent,
                                     Card   => (others => ' ')));

   --------------------------------------------------------------------
   --
   -- Local Subprograms
   --
   --------------------------------------------------------------------

   --------------------------------------------------------------------
   -- FindReader
   --
   -- Description:
   --    Returns the index of the named reader if we know about it.
   --
   -- Implementation Notes:
   --    None.
   --
   --------------------------------------------------------------------
   procedure FindReader (Name  : in     CommonTypes.String8T;
                         Id    :    out CommonTypes.String8ArrayI;
                         Found :    out Boolean)
   is
      FoundReader : Boolean := False;
      TheId : CommonTypes.String8ArrayI := CommonTypes.String8ArrayI'First;
   begin

      for Reader in CommonTypes.String8ArrayI loop

         FoundReader := Readers(Reader).Valid
           and then Readers(Reader).Name = Name;

         if Foundreader then
            TheId := Reader;
            exit;
         end if;

      end loop;

      Id := TheId;
      Found := FoundReader;

   end FindReader;


   --------------------------------------------------------------------
   -- FindReaderByCard
   --
   -- Description:
   --    Returns the index of the reader containing the given card.
   --
   -- Implementation Notes:
   --    None.
   --
   --------------------------------------------------------------------
   procedure FindReaderByCard (Name  : in     CardSim.CardNameT;
                               Id    :    out CommonTypes.String8ArrayI;
                               Found :    out Boolean)
   is
      FoundReader : Boolean := False;
      TheId : CommonTypes.String8ArrayI := CommonTypes.String8ArrayI'First;
   begin
      for Reader in CommonTypes.String8ArrayI loop

         FoundReader := Readers(Reader).Valid
           and then Readers(Reader).Card = Name;

         if Foundreader then
            TheId := Reader;
            exit;
         end if;

      end loop;

      Id := TheId;
      Found := FoundReader;

   end FindReaderByCard;


   --------------------------------------------------------------------
   -- NextFree
   --
   -- Description:
   --    Returns the first free reader index.
   --
   -- Implementation Notes:
   --    None.
   --
   --------------------------------------------------------------------
   function NextFree return CommonTypes.String8ArrayI
   is
      TheId : CommonTypes.String8ArrayI := CommonTypes.String8ArrayI'Last;
   begin

      for Reader in CommonTypes.String8ArrayI loop

         if not Readers(Reader).Valid then
            TheId := Reader;
            exit;
         end if;

      end loop;

      return TheId;

   end NextFree;


   --------------------------------------------------------------------
   -- SendListResponse
   --
   -- Description:
   --    Creates the response using the package state, and
   --    sends it to the client.
   --
   -- Implementation Notes:
   --    None.
   --
   --------------------------------------------------------------------
   procedure SendListResponse (Message : in TcpIp.MessageT;
                               IsAdmin : in Boolean;
                               Client  : in TcpIp.Sim.ClientT)
   is

      Char : TcpIp.MessageIndexT; -- points to next free char in the message

      NumReaders : Natural := 0;

      -- Not sure why this is really needed....
      ExpectedReaders : Natural :=
        Natural'Value(MsgProc.GetStringByPos (Message.Data, 1));

      Response : TcpIp.MessageT := TcpIp.NullMsg;

   begin
      -- Count the number of readers
      for Reader in Readers'Range loop
         if Readers(Reader).Valid then
            NumReaders := NumReaders + 1;
         end if;
      end loop;

      declare
         NumReadersString : String :=
           Ada.Strings.Fixed.Trim(Natural'Image(NumReaders), Ada.Strings.Both);
         NumLength        : Natural := NumReadersString'Length;
      begin
         Response.Data (1 .. 1) := "'";
         Response.Data (2 .. 2 + NumLength - 1) := NumReadersString;
         Char := 2 + NumLength;
         Response.Data (Char .. Char + 2) := "',{";
         Char := Char + 3;
      end;

      -- Now add the list of names...
      for Reader in Readers'Range loop
         if Readers(Reader).Valid then

            Response.Data (Char .. Char) := "'";
            Char := Char + 1;

            declare
               --Name : String := Ada.Strings.Fixed.Trim(Readers(Reader).Name,
                                                       --Ada.Strings.Both);
               Name : String := Readers(Reader).Name;
            begin
               Response.Data (Char .. Char + Name'Length + 1) := Name & "',";
               Char := Char + Name'Length + 2;
            end;

         end if;
      end loop;

      -- Don't want the last trailing comma...
      Char := Char - 1;

      Response.Data(Char) := '}';
      Response.Length := Char;
      TcpIp.Sim.Send ( IsAdmin => IsAdmin,
                       Client => Client,
                       Outgoing => Response);

   end SendListResponse;


   --------------------------------------------------------------------
   -- SendStateResponse
   --
   -- Description:
   --    Creates the response using the package state, and
   --    sends it to the client.
   --
   -- Implementation Notes:
   --    None.
   --
   --------------------------------------------------------------------
   procedure SendStateResponse (Message : in TcpIp.MessageT;
                                IsAdmin : in Boolean;
                                Client  : in TcpIp.Sim.ClientT)
   is
      Response   : TcpIp.MessageT := TcpIp.NullMsg;
      Reader     : String := MsgProc.GetStringByPos (Message.Data, 1);
      ReaderName : CommonTypes.String8T := (others => ' ');
      ReaderId   : CommonTypes.String8ArrayI;
      Found      : Boolean;
   begin

      -- Identify the reader
      ReaderName (1 .. Reader'Length) := Reader;
      FindReader (ReaderName, ReaderId, Found);

      if Found then

         declare

            CState : String :=
              Ada.Strings.Fixed.Trim
              (TokenAPI.CardStateT'Image(Readers(ReaderId).CState),
               Ada.Strings.Both);

            RState : String :=
              Ada.Strings.Fixed.Trim
              (TokenAPI.ReaderStateT'Image(Readers(ReaderId).RState),
               Ada.Strings.Both);

            CardHandle : String :=
              Ada.Strings.Fixed.Trim(Readers(ReaderId).Card, Ada.Strings.Both);

            Msg : String := "{'cState': '" & CState &
              "','cardHandle':'" & CardHandle &
              "','rState':'" & RState &
              "','name': '" & Reader & "'}";

         begin
            Response.Length := Msg'Length;
            Response.Data (1 .. Response.Length) := Msg;
         end;

      end if;

      TcpIp.Sim.Send ( IsAdmin  => IsAdmin,
                       Client   => Client,
                       Outgoing => Response);

   end SendStateResponse;


   --------------------------------------------------------------------
   -- SendConnectResponse
   --
   -- Description:
   --    Creates the response using the package state, and
   --    sends it to the client.
   --    Marks the card as connected.
   --
   -- Implementation Notes:
   --    None.
   --
   --------------------------------------------------------------------
   procedure SendConnectResponse (Message : in TcpIp.MessageT;
                                  IsAdmin : in Boolean;
                                  Client  : in TcpIp.Sim.ClientT)
   is
      Response    : TcpIp.MessageT := TcpIp.NullMsg;
      Reader      : String := MsgProc.GetStringByPos (Message.Data, 1);
      ReaderName  : CommonTypes.String8T := (others => ' ');
      ReaderId    : CommonTypes.String8ArrayI;
      Found       : Boolean;
      CardDetails : CardSim.CardDetails;
   begin

      -- Identify the reader
      ReaderName (1 .. Reader'Length) := Reader;
      FindReader (ReaderName, ReaderId, Found);

      if Found then

         CardSim.GetCard(Readers(ReaderId).Card, CardDetails, Found);

         if Found then

            -- Simulate successful connection to card
            Readers(ReaderId).CState := TokenAPI.Specific;

            -- Build response
            declare

               CardHandle : String :=
                 Ada.Strings.Fixed.Trim(CardDetails.Name, Ada.Strings.Both);

               Msg : String := "'" & CardHandle & "'";

            begin
               Response.Length := Msg'Length;
               Response.Data (1 .. Response.Length) := Msg;
            end;

         end if;

      end if;

      TcpIp.Sim.Send ( IsAdmin  => IsAdmin,
                       Client   => Client,
                       Outgoing => Response);

   end SendConnectResponse;


   --------------------------------------------------------------------
   -- SendStatusResponse
   --
   -- Description:
   --    Creates the response using the package state, and
   --    sends it to the client.
   --
   -- Implementation Notes:
   --    None.
   --
   --------------------------------------------------------------------
   procedure SendStatusResponse (Message : in TcpIp.MessageT;
                                 IsAdmin : in Boolean;
                                 Client  : in TcpIp.Sim.ClientT)
   is
      Response    : TcpIp.MessageT := TcpIp.NullMsg;
      Card        : String := MsgProc.GetStringByPos (Message.Data, 1);
      CardName    : CardSim.CardNameT := (others => ' ');
      CardDetails : CardSim.CardDetails;
      ReaderId    : CommonTypes.String8ArrayI;
      Found       : Boolean;
   begin

      -- Identify the reader
      CardName (1 .. Card'Length) := Card;
      FindReaderByCard (CardName, ReaderId, Found);
      if Found then

         CardSim.GetCard(Readers(ReaderId).Card, CardDetails, Found);

         if Found then

            declare

               ATR : String :=
                 Ada.Strings.Fixed.Trim
                 (CommonTypes.Unsigned32T'Image(CardDetails.ATR),
                  Ada.Strings.Both);

               CState : String :=
                 Ada.Strings.Fixed.Trim
                 (TokenAPI.CardStateT'Image(Readers(ReaderId).CState),
                  Ada.Strings.Both);

               Msg : String := "'" & CState & "','" & ATR & "'";

            begin
               Response.Length := Msg'Length;
               Response.Data (1 .. Response.Length) := Msg;
            end;

         end if;

      end if;

      TcpIp.Sim.Send ( IsAdmin  => IsAdmin,
                       Client   => Client,
                       Outgoing => Response);

   end SendStatusResponse;

   --------------------------------------------------------------------
   -- SendCert
   --    Looks up the given certificate on the given card and returns it
   --    to the client.
   --
   -- Description:
   --
   -- Implementation Notes:
   --    None.
   --
   --------------------------------------------------------------------
   procedure SendCert (Message : in TcpIp.MessageT;
                       IsAdmin : in Boolean;
                       Client  : in TcpIp.Sim.ClientT;
                       ReqCert : in BasicTypes.Unsigned32T)
   is
      Response    : TcpIp.MessageT := TcpIp.NullMsg;
      Card        : String := MsgProc.GetStringByPos (Message.Data, 1);
      CardName    : CardSim.CardNameT := (others => ' ');
      CardDetails : CardSim.CardDetails;
      ReaderId    : CommonTypes.String8ArrayI;
      Found       : Boolean;
   begin

      -- Identify the reader
      CardName (1 .. Card'Length) := Card;
      FindReaderByCard (CardName, ReaderId, Found);

      if Found then

         CardSim.GetCard(Readers(ReaderId).Card, CardDetails, Found);

         if Found then

            declare

               IDCert : String := CardDetails.IDCert.CertData(1..Integer(CardDetails.IDCert.CertLength));

               IACert : String := CardDetails.IACert.CertData(1..Integer(CardDetails.IACert.CertLength));

               AuthCert : String := CardDetails.AuthCert.CertData(1..Integer(CardDetails.AuthCert.CertLength));

               PrivCert : String := CardDetails.PrivCert.CertData(1..Integer(CardDetails.PrivCert.CertLength));

               Bool_Image : String := Boolean'Image(true);

            begin
               case ReqCert is
                  when CertProc.IDCertType =>
                     Response.Length := Bool_Image'Length + IDCert'Length + 5;
                     Response.Data (1 .. Response.Length) := "'" & Bool_Image & "',{" & IDCert & "}";
                  when CertProc.IACertType =>
                     Response.Length := Bool_Image'Length + IACert'Length + 5;
                     Response.Data (1 .. Response.Length) := "'" & Bool_Image & "',{" & IACert & "}";
                  when CertProc.PrivCertType =>
                     Response.Length := Bool_Image'Length + PrivCert'Length + 5;
                     Response.Data (1 .. Response.Length) := "'" & Bool_Image & "',{" & PrivCert & "}";
                  when CertProc.AuthCertType =>
                     Response.Length := Bool_Image'Length + Bool_Image'Length + AuthCert'Length + 8;
                     Response.Data (1 .. Response.Length) := "'" & Bool_Image & "','" &Bool_image& "',{" & AuthCert & "}";
                  when others =>
                     null;
               end case;
            end;

         end if;

      end if;

      TcpIp.Sim.Send ( IsAdmin  => IsAdmin,
                       Client   => Client,
                       Outgoing => Response);

   end SendCert;

   --------------------------------------------------------------------
   -- SetAuthCert
   --    Sets the authcert for the given card-id.
   --
   -- Description:
   --
   -- Implementation Notes:
   --    None.
   --
   --------------------------------------------------------------------
   procedure SetAuthCert (Message : in TcpIp.MessageT;
                          IsAdmin : in Boolean;
                          Client  : in TcpIp.Sim.ClientT)
   is
      Response    : TcpIp.MessageT := TcpIp.NullMsg;
      Card        : String := MsgProc.GetStringByPos (Message.Data, 1);
      CardName    : CardSim.CardNameT := (others => ' ');
      CardDetails : CardSim.CardDetails;
      ReaderId    : CommonTypes.String8ArrayI;
      Found       : Boolean;

      NewAuthCert : MsgProc.DictionaryT :=
              MsgProc.GetDictionary(Msg => Message.Data, Arg => 1);

   begin

      -- Identify the reader
      CardName (1 .. Card'Length) := Card;
      FindReaderByCard (CardName, ReaderId, Found);

      if Found then

         CardSim.GetCard(Readers(ReaderId).Card, CardDetails, Found);
         if Found then
            CardSim.SetAuthCert(Readers(ReaderId).Card, NewAuthCert);

            Response.Length := Boolean'Image(True)'Length + 3;
            Response.Data(1..Response.Length) := ",'" & Boolean'Image(True) & "'";
         end if;

      end if;

      TcpIp.Sim.Send ( IsAdmin  => IsAdmin,
                       Client   => Client,
                       Outgoing => Response);

   end SetAuthCert;

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
      ReaderName  : CommonTypes.String8T := (others => ' ');
      ReaderIndex : CommonTypes.String8ArrayI;
      CardName    : CardSim.CardNameT := (others => ' ');
      Found       : Boolean;
      LocalRState : TokenAPI.ReaderStateT;
      LocalCState : TokenAPI.CardStateT;
   begin

      if MsgProc.Sim.MatchCommand ( "insertCard", Message) then
         -- Get the details out of the message
         declare
            Reader : String := MsgProc.GetStringByPos (Message.Data, 1);
            Card   : String := Ada.Strings.Fixed.Trim(MsgProc.GetStringByPos (Message.Data, 2),
                                                      Ada.Strings.Both);
         begin
            ReaderName (1 .. Reader'Length) := Reader;
            CardName (1 .. Card'Length)     := Card;
         end;
         -- Identify the reader
         FindReader (ReaderName, ReaderIndex, Found);

         -- Update state
         if Found then
            Readers(ReaderIndex).RState := TokenAPI.Present;
            Readers(ReaderIndex).CState := TokenAPI.Present;
            Readers(ReaderIndex).Card   := CardName;
         end if;

         TcpIp.Sim.Send (IsAdmin, Client, Ack);

      elsif MsgProc.Sim.MatchCommand ( "removeCard", Message) then

         -- Get the details out of the message
         declare
            Reader : String := MsgProc.GetStringByPos (Message.Data, 1);
         begin
            ReaderName (1 .. Reader'Length) := Reader;
         end;

         -- Identify the reader
         FindReader (ReaderName, ReaderIndex, Found);

         -- Update state
         if Found then
            Readers(ReaderIndex).RState := TokenAPI.Empty;
            Readers(ReaderIndex).CState := TokenAPI.Absent;
            Readers(ReaderIndex).Card := (others => ' ');
         end if;

         TcpIp.Sim.Send (IsAdmin, Client, Ack);

      elsif MsgProc.Sim.MatchCommand ( "setState", Message) then

         -- Get the details out of the message
         declare

            Reader    : String := MsgProc.GetStringByPos (Message.Data, 1);
            StateDict : MsgProc.DictionaryT :=
              MsgProc.GetDictionary(Msg => Message.Data, Arg => 1);

         begin

            ReaderName (1 .. Reader'Length) := Reader;

            LocalRState := TokenAPI.ReaderStateT'Value
              (MsgProc.GetStringByKey(StateDict, "rState"));

            LocalCState := TokenAPI.CardStateT'Value
              (MsgProc.GetStringByKey(StateDict, "cState"));

         end;

         -- Do we already know about the reader?
         FindReader (ReaderName, ReaderIndex, Found);

         if not Found then
            ReaderIndex := NextFree;
         end if;

         -- Update state
         Readers(ReaderIndex).Name   := ReaderName;
         Readers(ReaderIndex).Valid  := True;
         Readers(ReaderIndex).RState := LocalRState;
         Readers(ReaderIndex).CState := LocalCState;

         TcpIp.Sim.Send (IsAdmin, Client, Ack);

      elsif MsgProc.Sim.MatchCommand ( "listReaders", Message) then

         SendListResponse (Message, IsAdmin, Client);

      elsif MsgProc.Sim.MatchCommand ( "getState", Message) then

         SendStateResponse (Message, IsAdmin, Client);

      elsif MsgProc.Sim.MatchCommand ( "connect", Message) then

         SendConnectResponse (Message, IsAdmin, Client);

      elsif MsgProc.Sim.MatchCommand ( "status", Message) then

         SendStatusResponse (Message, IsAdmin, Client);

      elsif MsgProc.Sim.MatchCommand ( "disconnect", Message) then

         -- Get the card Id
         declare
            Card : String := MsgProc.GetStringByPos (Message.Data, 1);
         begin
            CardName (1 .. Card'Length) := Card;
         end;

         FindReaderByCard (CardName, ReaderIndex, Found);

         -- Disconnect the card
         if Found then
            Readers(ReaderIndex).CState := TokenAPI.Present;
         end if;

         TcpIp.Sim.Send (IsAdmin, Client, Ack);

      elsif MsgProc.Sim.MatchCommand ( "getIDCert", Message) then

         SendCert (Message, IsAdmin, Client, CertProc.IDCertType);

      elsif MsgProc.Sim.MatchCommand ( "getPrivCert", Message) then

         SendCert (Message, IsAdmin, Client, CertProc.PrivCertType);

      elsif MsgProc.Sim.MatchCommand ( "getIACert", Message) then

         SendCert (Message, IsAdmin, Client, CertProc.IACertType);

      elsif MsgProc.Sim.MatchCommand ( "getAuthCert", Message) then

         SendCert (Message, IsAdmin, Client, CertProc.AuthCertType);

      elsif MsgProc.Sim.MatchCommand ( "updateAuthCert", Message) then

         SetAuthCert (Message, IsAdmin, Client);

      end if;


   end Handle;


end TokenReaderSim;
