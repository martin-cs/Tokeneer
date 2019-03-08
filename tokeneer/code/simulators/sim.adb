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
-- Sim main program
--
-- Description:
--    Main program for the SPRE simulators.
--
-- Implementation Notes:
--    Simple polling loop
------------------------------------------------------------------

with Ada.Text_IO;
with TcpIp.Sim;
with TcpIp;
with AlarmSim;
with BioDeviceSim;
with CardSim;
with DisplaySim;
with DoorSim;
with TokenReaderSim;
with MsgProc.Sim;

procedure Sim
is

   OK       : Boolean;
   Client   : TcpIp.Sim.ClientT;
   IsAdmin  : Boolean;
   Incoming : TcpIp.MessageT;

   ------------------------------------------------------------------
   -- InitTcpIp
   --
   -- Description:
   --    Performs the Initialisation of the TCP/IP interface.
   --
   -- Implementation Notes:
   --    None.
   --
   ------------------------------------------------------------------
   procedure InitTcpIp( Success : out Boolean)
   is
      TcpIpOK : Boolean;
   begin
      TcpIp.Sim.Init(TcpIpOK);
      if TcpIpOK then
         TcpIp.Sim.OpenAll;
      end if;
      if not TcpIpOK then
         Ada.Text_IO.Put_Line("Failed to initialise TCP/IP");
      end if;
      Success := TcpIpOK;
  end InitTcpIp;

   ------------------------------------------------------------------
   -- FinaliseTcpIp
   --
   -- Description:
   --    Performs the Finalisation of the TCP/IP interface.
   --
   -- Implementation Notes:
   --    None.
   --
   ------------------------------------------------------------------
   procedure FinaliseTcpIp
   is
   begin
      TcpIp.Sim.CloseAll;
   end FinaliseTcpIp;


   --------------------------------------------------------------
   -- begin Sim
   --------------------------------------------------------------
begin

   Ada.Text_IO.Put_Line ("Tokeneer ID Station peripheral simulator");
   Ada.Text_IO.Put_Line ("(c) Praxis High Integrity Systems 2008");

   InitTcpIp(Success => OK);

   if OK then

      Ada.Text_IO.Put_Line ("Network initialized OK - waiting for incoming message");

      loop -- Main Polling Loop

         TcpIp.Sim.Receive (IsAdmin, Client, Incoming, OK);

         if Ok then

            if MsgProc.Sim.MatchStart (DoorSim.Id, Incoming) then
               DoorSim.Handle (Incoming, IsAdmin, Client);
            elsif MsgProc.Sim.MatchStart (AlarmSim.Id, Incoming) then
               AlarmSim.Handle (Incoming, IsAdmin, Client);
            elsif MsgProc.Sim.MatchStart (BioDeviceSim.Id, Incoming) then
               BioDeviceSim.Handle (Incoming, IsAdmin, Client);
            elsif MsgProc.Sim.MatchStart (CardSim.Id, Incoming) then
               CardSim.Handle (Incoming, IsAdmin, Client);
            elsif MsgProc.Sim.MatchStart (DisplaySim.Id, Incoming) then
               DisplaySim.Handle (Incoming, IsAdmin, Client);
            elsif MsgProc.Sim.MatchStart (TokenReaderSim.Id, Incoming) then
               TokenReaderSim.Handle (Incoming, IsAdmin, Client);
            elsif MsgProc.Sim.MatchStart("ShutdownSim", Incoming) then
               exit;
            end if;

         end if;

      end loop;


   else
      Ada.Text_IO.Put_Line ("Network initialized failed");
   end if;

   FinaliseTcpIp;

exception
   when others =>
      Ada.Text_IO.Put_Line ("Top-level unhandled exception in sim");
      FinaliseTcpIp;
      raise;
end Sim;
