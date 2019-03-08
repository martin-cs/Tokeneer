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


with Gtk; use Gtk;
with Gdk.Types; use Gdk.Types;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Enums; use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Tokeneer_Gu_I; use Callbacks_Tokeneer_Gu_I;
with Tokeneer_Gu_I_Intl; use Tokeneer_Gu_I_Intl;
with Message_Box_Pkg.Callbacks; use Message_Box_Pkg.Callbacks;

package body Message_Box_Pkg is

Box : Message_Box_Access;

procedure Gtk_New (Message_Box : out Message_Box_Access) is
begin
   Message_Box := new Message_Box_Record;
   Message_Box_Pkg.Initialize (Message_Box);
   Box := Message_Box;
end Gtk_New;

procedure Initialize (Message_Box : access Message_Box_Record'Class) is
   pragma Suppress (All_Checks);
begin
   Gtk.Window.Initialize (Message_Box, Window_Toplevel);
   Set_Title (Message_Box, -"Message");
   Set_Position (Message_Box, Win_Pos_Center);
   Set_Modal (Message_Box, True);

   Gtk_New_Vbox (Message_Box.Vbox2, False, 3);
   Add (Message_Box, Message_Box.Vbox2);

   Gtk_New (Message_Box.Message_Label, -("Initial Text"));
   Set_Alignment (Message_Box.Message_Label, 0.5, 0.5);
   Set_Padding (Message_Box.Message_Label, 0, 0);
   Set_Justify (Message_Box.Message_Label, Justify_Left);
   Set_Line_Wrap (Message_Box.Message_Label, False);
   Set_Selectable (Message_Box.Message_Label, False);
   Set_Use_Markup (Message_Box.Message_Label, False);
   Set_Use_Underline (Message_Box.Message_Label, False);
   Pack_Start
     (Message_Box.Vbox2,
      Message_Box.Message_Label,
      Expand  => False,
      Fill    => False,
      Padding => 0);

   Gtk_New_Hseparator (Message_Box.Hseparator1);
   Pack_Start
     (Message_Box.Vbox2,
      Message_Box.Hseparator1,
      Expand  => True,
      Fill    => True,
      Padding => 0);

   Gtk_New_Hbox (Message_Box.Hbox1, True, 0);
   Pack_Start
     (Message_Box.Vbox2,
      Message_Box.Hbox1,
      Expand  => True,
      Fill    => True,
      Padding => 0);

   Gtk_New_From_Stock (Message_Box.Message_Ok, "gtk-ok");
   Set_Relief (Message_Box.Message_Ok, Relief_Normal);
   Pack_Start
     (Message_Box.Hbox1,
      Message_Box.Message_Ok,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Button_Callback.Connect
     (Message_Box.Message_Ok, "clicked",
      Button_Callback.To_Marshaller (On_Message_Ok_Clicked'Access), False);

end Initialize;

procedure Show_Message(Text: in UTF8_String)
is
   Tmp_Box: Message_Box_Access;
begin
   -- Make sure we have a message box to use...
   if Box = null then
      Gtk_New(Tmp_Box);
      Initialize(Tmp_Box);
   end if;
   Set_Text(Box.Message_Label, Text);
   Show_All(Box);
   Present(Box);
end Show_Message;

end Message_Box_Pkg;
