------------------------------------------------------------------------------
-- Tokeneer ID Station - GUI
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


with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Main;
with Message_Box_Pkg;
with GNAT.OS_Lib; use Gnat.Os_Lib;
with Enclave_Pkg;
with Ada.Command_Line;
with GNAT.Sockets;

package body Tis_Main_Pkg.Callbacks is

   use Gtk.Arguments;

   --------------------------
   -- On_Start_Sim_Clicked --
   --------------------------

   procedure On_Start_Sim_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      PID: Gnat.OS_Lib.Process_ID;
      Args: Gnat.Os_Lib.Argument_List :=
         Gnat.OS_Lib.Argument_String_To_List("/c start " & Enclave_Pkg.Default_Dir & "sim").All;
   begin
      -- Try to spwan the Simulator as a seperate process using GNAT.OS_Lib:
      PID := Gnat.OS_Lib.Non_Blocking_Spawn("cmd", Args);

      if PID = Gnat.Os_Lib.Invalid_Pid then
         Message_Box_Pkg.Show_Message("Error starting Simulators -- please Exit and restart");
      else
         Message_Box_Pkg.Show_Message("Simulators started - please wait for DOS box before continuing");
      end if;
   end On_Start_Sim_Clicked;

   ----------------------------
   -- On_Sim_Connect_Clicked --
   ----------------------------

   procedure On_Sim_Connect_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Enclave_Pkg.Start_Enclave_Gui;
   end On_Sim_Connect_Clicked;

   --------------------------
   -- On_Start_Tis_Clicked --
   --------------------------
   function Get_Server return String
   is
   begin
      if Ada.Command_Line.Argument_Count = 1 then
         return Ada.Command_Line.Argument(1);
      else
         return GNAT.Sockets.Host_Name;
      end if;
   end Get_Server;

   procedure On_Start_Tis_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      PID: Gnat.OS_Lib.Process_ID;
      Args: Gnat.Os_Lib.Argument_List :=
         Gnat.OS_Lib.Argument_String_To_List("/c " & Enclave_Pkg.Default_Dir & "tis " & Get_Server ).All;
   begin
      -- Try to spwan the main TIS application as a seperate process using GNAT.OS_Lib:
      PID := Gnat.OS_Lib.Non_Blocking_Spawn("cmd", Args);

      if PID = Gnat.Os_Lib.Invalid_Pid then
         Message_Box_Pkg.Show_Message("Error starting TIS -- please Exit and restart");
      else
         Message_Box_Pkg.Show_Message("TIS started - please wait for DOS box before continuing");
      end if;
   end On_Start_Tis_Clicked;

   ---------------------
   -- On_Exit_Clicked --
   ---------------------

   procedure On_Exit_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      -- Unconditional exit
      Gtk.Main.Main_Quit;
   end On_Exit_Clicked;

end Tis_Main_Pkg.Callbacks;

