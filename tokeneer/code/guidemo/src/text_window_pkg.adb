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
with Text_Window_Pkg.Callbacks; use Text_Window_Pkg.Callbacks;

package body Text_Window_Pkg is

procedure Gtk_New (Text_Window : out Text_Window_Access) is
begin
   Text_Window := new Text_Window_Record;
   Text_Window_Pkg.Initialize (Text_Window);
end Gtk_New;

procedure Initialize (Text_Window : access Text_Window_Record'Class) is
   pragma Suppress (All_Checks);
begin
   Gtk.Window.Initialize (Text_Window, Window_Toplevel);
   Set_Title (Text_Window, -"TextWindow");
   Set_Position (Text_Window, Win_Pos_None);
   Set_Modal (Text_Window, False);

   Gtk_New (Text_Window.Fixed1);
   Add (Text_Window, Text_Window.Fixed1);

   Gtk_New (Text_Window.Scrolledwindow1);
   Set_Policy (Text_Window.Scrolledwindow1, Policy_Always, Policy_Always);
   Put (Text_Window.Fixed1, Text_Window.Scrolledwindow1, 0, 0);

   Gtk_New (Text_Window.Textview1);
   Set_Editable (Text_Window.Textview1, True);
   Set_Justification (Text_Window.Textview1, Justify_Left);
   Set_Wrap_Mode (Text_Window.Textview1, Wrap_Word);
   Set_Cursor_Visible (Text_Window.Textview1, False);
   Set_Pixels_Above_Lines (Text_Window.Textview1, 0);
   Set_Pixels_Below_Lines (Text_Window.Textview1, 0);
   Set_Pixels_Inside_Wrap (Text_Window.Textview1, 0);
   Set_Left_Margin (Text_Window.Textview1, 0);
   Set_Right_Margin (Text_Window.Textview1, 0);
   Set_Indent (Text_Window.Textview1, 0);
   declare
      Iter : Gtk_Text_Iter;
   begin
      Get_Iter_At_Line (Get_Buffer (Text_Window.Textview1), Iter, 0);
      Insert (Get_Buffer (Text_Window.Textview1), Iter,
         -(""));
   end;
   Add (Text_Window.Scrolledwindow1, Text_Window.Textview1);

   Gtk_New (Text_Window.Button3, -"button3");
   Set_Relief (Text_Window.Button3, Relief_Normal);
   Put (Text_Window.Fixed1, Text_Window.Button3, 136, 272);
   Button_Callback.Connect
     (Text_Window.Button3, "clicked",
      Button_Callback.To_Marshaller (On_Button3_Clicked'Access), False);

   Gtk_New (Text_Window.Button1, -"button1");
   Set_Relief (Text_Window.Button1, Relief_Normal);
   Put (Text_Window.Fixed1, Text_Window.Button1, 8, 272);
   Button_Callback.Connect
     (Text_Window.Button1, "clicked",
      Button_Callback.To_Marshaller (On_Button1_Clicked'Access), False);

   Gtk_New (Text_Window.Button2, -"button2");
   Set_Relief (Text_Window.Button2, Relief_Normal);
   Put (Text_Window.Fixed1, Text_Window.Button2, 72, 272);
   Button_Callback.Connect
     (Text_Window.Button2, "clicked",
      Button_Callback.To_Marshaller (On_Button2_Clicked'Access), False);

end Initialize;

end Text_Window_Pkg;

