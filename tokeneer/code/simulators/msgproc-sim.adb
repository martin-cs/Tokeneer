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
-- MsgProc.Sim
--
-- Implementation Notes:
--    None.
--
------------------------------------------------------------------

package body MsgProc.Sim
is


   ------------------------------------------------------------------
   -- MatchStart
   --
   -- Implementation Notes:
   --    None.
   --
   ------------------------------------------------------------------
   function MatchStart (Text    : String;
                        Message : TcpIp.MessageT) return Boolean
   is
      IsMatch : Boolean := False;
   begin


      if Text'Length <= Message.Length then

         for CharNum in Text'Range loop

            IsMatch := Message.Data(CharNum) = Text(CharNum);
            exit when not IsMatch;

         end loop;

      end if;

      return IsMatch;

   end MatchStart;


   ------------------------------------------------------------------
   -- MatchCommand
   --
   -- Implementation Notes:
   --    None.
   --
   ------------------------------------------------------------------
   function MatchCommand (Text    : String;
                          Message : TcpIp.MessageT) return Boolean
   is
      IsMatch  : Boolean := False;
      DotAt    : Natural := 0;
   begin


      -- Find the dot
      for CharNum in 1 .. Message.Length loop

         if Message.Data(CharNum) = '.' then

            DotAt := Charnum;
            exit;

         end if;

      end loop;

      -- Does text after dot match?
      if Text'Length <= Message.Length - DotAt then

         for CharNum in Text'Range loop

            IsMatch := Message.Data(CharNum + DotAt) = Text(CharNum);
            exit when not IsMatch;

         end loop;

      end if;

      return IsMatch;

   end MatchCommand;


end MsgProc.Sim;
