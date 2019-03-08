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
-- Description:
--    Maintains a database of cards.
--
------------------------------------------------------------------

with CommonTypes;
with TcpIp;
with TcpIp.Sim;
with TokenAPI;
with MsgProc;

package CardSim
is

   Id : constant String := "cardDB";

   -- 32 bit number = 10 digits, plus 'p' for Praxis!
   subtype CardNameI is Natural range 1 .. 11;
   subtype CardNameT is String (CardNameI);

   type CardDetails is record
      Name     : CardNameT;
      Valid    : Boolean;
      ATR      : CommonTypes.Unsigned32T;
      IDCert   : TokenAPI.GenericRawCertT;
      PrivCert : TokenAPI.GenericRawCertT;
      IACert   : TokenAPI.GenericRawCertT;
      AuthCert : TokenAPI.GenericRawCertT;
   end record;

   --------------------------------------------------------------------
   -- Handle
   --
   -- Description:
   --    Parses the input message and reacts accordingly, updating
   --    state and sending the appropriate response to the client.
   --
   --------------------------------------------------------------------
   procedure Handle (Message : in TcpIp.MessageT;
                     IsAdmin : in Boolean;
                     Client  : in TcpIp.Sim.ClientT);


   --------------------------------------------------------------------
   -- GetCard
   --
   -- Description:
   --    Returns the details for a given card.
   --
   --------------------------------------------------------------------
   procedure GetCard (Name    : in     CardNameT;
                      Details :    out CardDetails;
                      Found   :    out Boolean);

   --------------------------------------------------------------------
   -- SetAuthCert
   --
   -- Description:
   --    Sets the AuthCert for a given card
   --
   --------------------------------------------------------------------
   procedure SetAuthCert (Name    : in     CardNameT;
                          NewCert : in MsgProc.DictionaryT);


end CardSim;
