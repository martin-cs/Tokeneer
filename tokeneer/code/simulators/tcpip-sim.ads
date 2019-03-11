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
-- Description:
--    Provides the operations to communicate with the SPRE drivers
--    using TCP/IP.
--
------------------------------------------------------------------

package TcpIp.Sim is

   -- The number of different open clients we can listen to
   type ClientT is range 1 .. 100;

   --------------------------------------------------------------------
   -- OpenAll
   --
   -- Description:
   --    Opens TCP/IP connections to both the Portal and Admin ports.
   --
   --------------------------------------------------------------------

   procedure OpenAll;


   --------------------------------------------------------------------
   -- CloseAll
   --
   -- Description:
   --    Closes the TCP/IP connection with both the Portal and Admin ports.
   --
   --------------------------------------------------------------------

   procedure CloseAll;


   --------------------------------------------------------------------
   -- Send
   --
   -- Description:
   --    Sends a response to TIS. Success will be false if there is a
   --    communication error.
   --    Outgoing does not include the message delineation sequence:
   --    This procedure appends the sequence to the Outgoing string.
   --
   --------------------------------------------------------------------

   procedure Send ( IsAdmin  : in     Boolean;
                    Client   : in     ClientT;
                    Outgoing : in     TcpIp.MessageT);

   --------------------------------------------------------------------
   -- Receive
   --
   -- Description:
   --    Receives a message from either TIS or TestTIS.
   --
   --------------------------------------------------------------------

   procedure Receive ( IsAdmin  :    out Boolean;
                       Client   :    out ClientT;
                       Incoming :    out MessageT;
                       Success  :    out Boolean);

   --------------------------------------------------------------------
   -- Init
   --
   -- Description:
   --    Extracts from the command line any arguments that reset the
   --    default machine name and ports for the Test Devices.
   --
   --------------------------------------------------------------------

   procedure Init ( Success  :    out Boolean);

end TcpIp.Sim;
