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
with Callbacks_Project2; use Callbacks_Project2;
with Project2_Intl; use Project2_Intl;
with Text_Window2_Pkg.Callbacks; use Text_Window2_Pkg.Callbacks;

package body Text_Window2_Pkg is

procedure Gtk_New (Text_Window2 : out Text_Window2_Access) is
begin
   Text_Window2 := new Text_Window2_Record;
   Text_Window2_Pkg.Initialize (Text_Window2);
end Gtk_New;

procedure Initialize (Text_Window2 : access Text_Window2_Record'Class) is
   pragma Suppress (All_Checks);
begin
   Gtk.Window.Initialize (Text_Window2, Window_Toplevel);
   Set_Title (Text_Window2, -"Window2");
   Set_Position (Text_Window2, Win_Pos_None);
   Set_Modal (Text_Window2, False);

   Gtk_New_Vbox (Text_Window2.Vbox1, False, 0);
   Add (Text_Window2, Text_Window2.Vbox1);

   Gtk_New (Text_Window2.Scrolledwindow2);
   Set_Policy (Text_Window2.Scrolledwindow2, Policy_Automatic, Policy_Always);
   Pack_Start
     (Text_Window2.Vbox1,
      Text_Window2.Scrolledwindow2,
      Expand  => True,
      Fill    => True,
      Padding => 0);

   Gtk_New (Text_Window2.Textview2);
   Set_Editable (Text_Window2.Textview2, True);
   Set_Justification (Text_Window2.Textview2, Justify_Left);
   Set_Wrap_Mode (Text_Window2.Textview2, Wrap_None);
   Set_Cursor_Visible (Text_Window2.Textview2, True);
   Set_Pixels_Above_Lines (Text_Window2.Textview2, 0);
   Set_Pixels_Below_Lines (Text_Window2.Textview2, 0);
   Set_Pixels_Inside_Wrap (Text_Window2.Textview2, 0);
   Set_Left_Margin (Text_Window2.Textview2, 0);
   Set_Right_Margin (Text_Window2.Textview2, 0);
   Set_Indent (Text_Window2.Textview2, 0);
   declare
      Iter : Gtk_Text_Iter;
   begin
      Get_Iter_At_Line (Get_Buffer (Text_Window2.Textview2), Iter, 0);
      Insert (Get_Buffer (Text_Window2.Textview2), Iter,
         -(""));
   end;
   Add (Text_Window2.Scrolledwindow2, Text_Window2.Textview2);

   Gtk_New (Text_Window2.Hbuttonbox1);
   Set_Spacing (Text_Window2.Hbuttonbox1, 0);
   Set_Layout (Text_Window2.Hbuttonbox1, Buttonbox_Default_Style);
   Pack_End
     (Text_Window2.Vbox1,
      Text_Window2.Hbuttonbox1,
      Expand  => False,
      Fill    => False,
      Padding => 0);

   Gtk_New (Text_Window2.Button4, -"button4");
   Set_Relief (Text_Window2.Button4, Relief_Normal);
   Set_Flags (Text_Window2.Button4, Can_Default);
   Button_Callback.Connect
     (Text_Window2.Button4, "clicked",
      Button_Callback.To_Marshaller (On_Button4_Clicked'Access), False);
   Pack_Start (Text_Window2.Hbuttonbox1, Text_Window2.Button4);

   Gtk_New (Text_Window2.Button5, -"button5");
   Set_Relief (Text_Window2.Button5, Relief_Normal);
   Set_Flags (Text_Window2.Button5, Can_Default);
   Button_Callback.Connect
     (Text_Window2.Button5, "clicked",
      Button_Callback.To_Marshaller (On_Button5_Clicked'Access), False);
   Pack_Start (Text_Window2.Hbuttonbox1, Text_Window2.Button5);

   Gtk_New (Text_Window2.Button6, -"button6");
   Set_Relief (Text_Window2.Button6, Relief_Normal);
   Set_Flags (Text_Window2.Button6, Can_Default);
   Button_Callback.Connect
     (Text_Window2.Button6, "clicked",
      Button_Callback.To_Marshaller (On_Button6_Clicked'Access), False);
   Pack_Start (Text_Window2.Hbuttonbox1, Text_Window2.Button6);

end Initialize;

end Text_Window2_Pkg;

