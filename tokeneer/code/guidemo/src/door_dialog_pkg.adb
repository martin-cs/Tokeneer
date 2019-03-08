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
with Door_Dialog_Pkg.Callbacks; use Door_Dialog_Pkg.Callbacks;

package body Door_Dialog_Pkg is

procedure Gtk_New (Door_Dialog : out Door_Dialog_Access) is
begin
   Door_Dialog := new Door_Dialog_Record;
   Door_Dialog_Pkg.Initialize (Door_Dialog);
end Gtk_New;

procedure Initialize (Door_Dialog : access Door_Dialog_Record'Class) is
   pragma Suppress (All_Checks);
begin
   Gtk.Window.Initialize (Door_Dialog, Window_Toplevel);
   Set_Title (Door_Dialog, -"Open Door");
   Set_Position (Door_Dialog, Win_Pos_Center);
   Set_Modal (Door_Dialog, True);

   Gtk_New_Vbox (Door_Dialog.Vbox6, False, 0);
   Add (Door_Dialog, Door_Dialog.Vbox6);

   Gtk_New (Door_Dialog.Label13, -("The door is closed." & ASCII.LF
& "Do you wish to force the door?"));
   Set_Alignment (Door_Dialog.Label13, 0.5, 0.5);
   Set_Padding (Door_Dialog.Label13, 6, 6);
   Set_Justify (Door_Dialog.Label13, Justify_Center);
   Set_Line_Wrap (Door_Dialog.Label13, False);
   Set_Selectable (Door_Dialog.Label13, False);
   Set_Use_Markup (Door_Dialog.Label13, False);
   Set_Use_Underline (Door_Dialog.Label13, False);
   Pack_Start
     (Door_Dialog.Vbox6,
      Door_Dialog.Label13,
      Expand  => False,
      Fill    => False,
      Padding => 0);

   Gtk_New_Hbox (Door_Dialog.Hbox3, True, 0);
   Set_Border_Width (Door_Dialog.Hbox3, 12);
   Pack_Start
     (Door_Dialog.Vbox6,
      Door_Dialog.Hbox3,
      Expand  => True,
      Fill    => True,
      Padding => 0);

   Gtk_New (Door_Dialog.Label14);
   Set_Alignment (Door_Dialog.Label14, 0.5, 0.5);
   Set_Padding (Door_Dialog.Label14, 0, 0);
   Set_Justify (Door_Dialog.Label14, Justify_Left);
   Set_Line_Wrap (Door_Dialog.Label14, False);
   Set_Selectable (Door_Dialog.Label14, False);
   Set_Use_Markup (Door_Dialog.Label14, False);
   Set_Use_Underline (Door_Dialog.Label14, False);
   Pack_Start
     (Door_Dialog.Hbox3,
      Door_Dialog.Label14,
      Expand  => False,
      Fill    => False,
      Padding => 0);

   Gtk_New (Door_Dialog.Yes, -"Yes");
   Set_Relief (Door_Dialog.Yes, Relief_Normal);
   Pack_Start
     (Door_Dialog.Hbox3,
      Door_Dialog.Yes,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Button_Callback.Connect
     (Door_Dialog.Yes, "clicked",
      Button_Callback.To_Marshaller (On_Yes_Clicked'Access), False);

   Gtk_New (Door_Dialog.No, -"No");
   Set_Relief (Door_Dialog.No, Relief_Normal);
   Pack_Start
     (Door_Dialog.Hbox3,
      Door_Dialog.No,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Button_Callback.Connect
     (Door_Dialog.No, "clicked",
      Button_Callback.To_Marshaller (On_No_Clicked'Access), False);

   Gtk_New (Door_Dialog.Label15);
   Set_Alignment (Door_Dialog.Label15, 0.5, 0.5);
   Set_Padding (Door_Dialog.Label15, 0, 0);
   Set_Justify (Door_Dialog.Label15, Justify_Left);
   Set_Line_Wrap (Door_Dialog.Label15, False);
   Set_Selectable (Door_Dialog.Label15, False);
   Set_Use_Markup (Door_Dialog.Label15, False);
   Set_Use_Underline (Door_Dialog.Label15, False);
   Pack_Start
     (Door_Dialog.Hbox3,
      Door_Dialog.Label15,
      Expand  => False,
      Fill    => False,
      Padding => 0);

end Initialize;

procedure Ask
is
   TmpDoor: Door_Dialog_Access;
begin
   Gtk_New(TmpDoor);
   Show_All(TmpDoor);
end Ask;

end Door_Dialog_Pkg;
