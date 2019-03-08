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


with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types; use Gdk.Types;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Enums; use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Tokeneer_Gu_I; use Callbacks_Tokeneer_Gu_I;
with Tokeneer_Gu_I_Intl; use Tokeneer_Gu_I_Intl;
with Tis_Main_Pkg.Callbacks; use Tis_Main_Pkg.Callbacks;
with GNAT.Sockets;

package body Tis_Main_Pkg is

procedure Gtk_New (Tis_Main : out Tis_Main_Access) is
begin
   Tis_Main := new Tis_Main_Record;
   Tis_Main_Pkg.Initialize (Tis_Main);
end Gtk_New;

procedure Initialize (Tis_Main : access Tis_Main_Record'Class) is
   pragma Suppress (All_Checks);
begin
   GNAT.Sockets.Initialize; -- so we can safely call Host_Name

   Gtk.Window.Initialize (Tis_Main, Window_Toplevel);
   Set_Title (Tis_Main, -"TIS Demo");
   Set_Position (Tis_Main, Win_Pos_None);
   Set_Modal (Tis_Main, False);

   Gtk_New (Tis_Main.Vbuttonbox1);
   Set_Spacing (Tis_Main.Vbuttonbox1, 0);
   Set_Layout (Tis_Main.Vbuttonbox1, Buttonbox_Default_Style);
   Add (Tis_Main, Tis_Main.Vbuttonbox1);

   Gtk_New (Tis_Main.Start_Sim, -"Start Simulators");
   Set_Relief (Tis_Main.Start_Sim, Relief_Normal);
   Set_Flags (Tis_Main.Start_Sim, Can_Default);
   Button_Callback.Connect
     (Tis_Main.Start_Sim, "clicked",
      Button_Callback.To_Marshaller (On_Start_Sim_Clicked'Access), False);
   Pack_Start (Tis_Main.Vbuttonbox1, Tis_Main.Start_Sim);

   Gtk_New (Tis_Main.Sim_Connect, -"Connect To Simulator");
   Set_Relief (Tis_Main.Sim_Connect, Relief_Normal);
   Set_Flags (Tis_Main.Sim_Connect, Can_Default);
   Button_Callback.Connect
     (Tis_Main.Sim_Connect, "clicked",
      Button_Callback.To_Marshaller (On_Sim_Connect_Clicked'Access), False);
   Pack_Start (Tis_Main.Vbuttonbox1, Tis_Main.Sim_Connect);

   Gtk_New (Tis_Main.Start_Tis, -"Start TIS");
   Set_Relief (Tis_Main.Start_Tis, Relief_Normal);
   Set_Flags (Tis_Main.Start_Tis, Can_Default);
   Button_Callback.Connect
     (Tis_Main.Start_Tis, "clicked",
      Button_Callback.To_Marshaller (On_Start_Tis_Clicked'Access), False);
   Pack_Start (Tis_Main.Vbuttonbox1, Tis_Main.Start_Tis);

   Gtk_New (Tis_Main.Exit_Button, -"Exit");
   Set_Relief (Tis_Main.Exit_Button, Relief_Normal);
   Set_Flags (Tis_Main.Exit_Button, Can_Default);
   Button_Callback.Connect
     (Tis_Main.Exit_Button, "clicked",
      Button_Callback.To_Marshaller (On_Exit_Clicked'Access), False);
   Pack_Start (Tis_Main.Vbuttonbox1, Tis_Main.Exit_Button);

end Initialize;

end Tis_Main_Pkg;

