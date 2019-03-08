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
-- CardSim
--
-- Implementation Notes:
--    Two thoughts:
--    1) The CardDb could be initialised containing details of all
--       the cards in the Praxis tests; and/or
--    2) The CardDb could be made persistent by writing it to a
--       file each time a card is added, and reading that file
--       in (if it exists) at initialisation.
--
------------------------------------------------------------------

with Ada.Strings.Fixed;
with MsgProc.Sim;

package body CardSim
is

   Ack : constant TcpIp.MessageT := TcpIp.NullMsg;

   --------------------------------------------------------------------
   -- State
   --------------------------------------------------------------------

   -- Arbitrary size of datsbase = 100
   type CardDbI is range 1 .. 100;
   type CardDbT is array (CardDbI) of CardDetails;

   NullDetails : CardDetails := (Name     => (others => ' '),
                                 Valid    => False,
                                 IDCert   => TokenAPI.NullGenericRawCert,
                                 ATR      => CommonTypes.Unsigned32T'First,
                                 PrivCert => TokenAPI.NullGenericRawCert,
                                 IACert   => TokenAPI.NullGenericRawCert,
                                 AuthCert => TokenAPI.NullGenericRawCert);

   CardDb : CardDbT := (others => NullDetails);


   --------------------------------------------------------------------
   --
   -- Local Subprograms
   --
   --------------------------------------------------------------------

   --------------------------------------------------------------------
   -- FindCard
   --
   -- Description:
   --    Returns the ID of the named card if it is in the database.
   --
   -- Implementation Notes:
   --    None.
   --
   --------------------------------------------------------------------
   procedure FindCard (Name  : in     CardNameT;
                       Id    :    out CardDbI;
                       Found :    out Boolean)
   is
      FoundCard : Boolean := False;
      TheId : CardDbI := CardDbI'First;
   begin

      for Card in CardDbI loop

         FoundCard := CardDb(Card).Valid and then CardDb(Card).Name = Name;

         if Foundcard then
            TheId := Card;
            exit;
         end if;

      end loop;

      Id := TheId;
      Found := FoundCard;

   end FindCard;


   --------------------------------------------------------------------
   -- NextFree
   --
   -- Description:
   --    Returns the first free ID in the database.
   --
   -- Implementation Notes:
   --    None.
   --
   --------------------------------------------------------------------
   function NextFree return CardDbI
   is
      TheId : CardDbI := CardDbI'Last;
   begin

      for Card in CardDbI loop

         if not  CardDb(Card).Valid then
            TheId := Card;
            exit;
         end if;

      end loop;

      return TheId;

   end NextFree;


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
      TheCardName : CardNameT := (others => ' ');
      TheCardId   : CardDbI;
      Found       : Boolean;
      CertDataKey : constant String := "CertDataT";

      MsgString   : String := Message.Data (1 .. Message.Length);

      ATR           : CommonTypes.Unsigned32T;
      LocalIDCert   : TokenAPI.GenericRawCertT := TokenAPI.NullGenericRawCert;
      LocalPrivCert : TokenAPI.GenericRawCertT := TokenAPI.NullGenericRawCert;
      LocalIACert   : TokenAPI.GenericRawCertT := TokenAPI.NullGenericRawCert;
      LocalAuthCert : TokenAPI.GenericRawCertT := TokenAPI.NullGenericRawCert;
   begin
      if MsgProc.Sim.MatchCommand ( "putCard", Message) then

         -- Get the details out of the message
         declare

            CardName : String := Ada.Strings.Fixed.Trim(MsgProc.GetStringByPos (MsgString, 1), Ada.Strings.Both);

            RawMsg : MsgProc.DictionaryT :=
              MsgProc.GetDictionary (MsgString, 1);

            RawIDCert : MsgProc.DictionaryT :=
              MsgProc.GetDictionaryByKey (RawMsg, "RawIDCert");
            RawAuthCert : MsgProc.DictionaryT :=
              MsgProc.GetDictionaryByKey (RawMsg, "RawAuthCert");
            RawIACert : MsgProc.DictionaryT :=
              MsgProc.GetDictionaryByKey (RawMsg, "RawIACert");
            RawPrivCert : MsgProc.DictionaryT :=
              MsgProc.GetDictionaryByKey (RawMsg, "RawPrivCert");

            --IDCert : MsgProc.DictionaryT :=
              --MsgProc.GetDictionaryByKey (RawIDCert, CertDataKey);
            --AuthCert : MsgProc.DictionaryT :=
              --MsgProc.GetDictionaryByKey (RawAuthCert, CertDataKey);
            --IACert : MsgProc.DictionaryT :=
              --MsgProc.GetDictionaryByKey (RawIACert, CertDataKey);
            --PrivCert : MsgProc.DictionaryT :=
              --MsgProc.GetDictionaryByKey (RawPrivCert, CertDataKey);

            IDCert : MsgProc.DictionaryT := RawIDCert;
            AuthCert : MsgProc.DictionaryT := RawAuthCert;
            IACert : MsgProc.DictionaryT := RawIACert;
            PrivCert : MsgProc.DictionaryT := RawPrivCert;
         begin

            TheCardName (1 .. CardName'Length) := CardName;

            ATR := CommonTypes.Unsigned32T'Value
              (Ada.Strings.Fixed.Trim(MsgProc.GetStringByKey(RawMsg, "ATR"),
                                      Ada.Strings.Both));

            LocalIDCert.CertLength   := IDCert'Length;
            LocalPrivCert.CertLength := PrivCert'Length;
            LocalIACert.CertLength   := IACert'Length;
            LocalAuthCert.CertLength := AuthCert'Length;

            LocalIDCert.CertData   (1 .. IDCert'Length)   := String(IDCert);
            LocalPrivCert.CertData (1 .. PrivCert'Length) := String(PrivCert);
            LocalIACert.CertData   (1 .. IACert'Length)   := String(IACert);
            LocalAuthCert.CertData (1 .. AuthCert'Length) := String(AuthCert);

         end;

         -- Is the card in the database?
         FindCard (TheCardName, TheCardId, Found);

         if not Found then
            TheCardId := NextFree;
         end if;

         -- Update the database with the details
         CardDb(TheCardId) := (Name     => TheCardName,
                               Valid    => True,
                               ATR      => ATR,
                               IDCert   => LocalIDCert,
                               PrivCert => LocalPrivCert,
                               IACert   => LocalIACert,
                               AuthCert => LocalAuthCert);

         TcpIp.Sim.Send (IsAdmin, Client, Ack);

      end if;


   end Handle;

   --------------------------------------------------------------------
   -- GetCard
   --
   -- Implementation Notes:
   --    None.
   --
   --------------------------------------------------------------------
   procedure GetCard (Name    : in     CardNameT;
                      Details :    out CardDetails;
                      Found   :    out Boolean)
   is
      Id        : CardDbI;
      CardFound : Boolean;
   begin

      FindCard (Name, Id, CardFound);
      if CardFound then
         Details := CardDb(Id);
      else
         Details := NullDetails;
      end if;
      Found := CardFound;

   end GetCard;

   --------------------------------------------------------------------
   -- SetAuthCert
   --
   -- Implementation Notes:
   --    The given card must already exist in the database.  If the card
   --    does not exist, nothing happens.
   --
   --------------------------------------------------------------------
   procedure SetAuthCert (Name    : in CardNameT;
                          NewCert : in MsgProc.DictionaryT)
   is
      Id        : CardDbI;
      CardFound : Boolean;
   begin

      FindCard (Name, Id, CardFound);
      if CardFound then
         CardDb(Id).AuthCert.CertLength := NewCert'Length;
         CardDb(Id).AuthCert.CertData(1..NewCert'Length) := String(NewCert);
      end if;
   end SetAuthCert;

end CardSim;
