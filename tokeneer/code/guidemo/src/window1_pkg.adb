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

package body Window1_Pkg is

procedure Gtk_New (Window1 : out Window1_Access) is
begin
   Window1 := new Window1_Record;
   Window1_Pkg.Initialize (Window1);
end Gtk_New;

procedure Initialize (Window1 : access Window1_Record'Class) is
   pragma Suppress (All_Checks);
begin
   Gtk.Window.Initialize (Window1, Window_Toplevel);
   Set_Title (Window1, -"window1");
   Set_Position (Window1, Win_Pos_None);
   Set_Modal (Window1, False);

   Gtk_New (Window1.Fixed1);
   Add (Window1, Window1.Fixed1);

   Gtk_New (Window1.Button1, -"button1");
   Set_Relief (Window1.Button1, Relief_Normal);
   Put (Window1.Fixed1, Window1.Button1, 8, 272);

   Gtk_New (Window1.Button2, -"button2");
   Set_Relief (Window1.Button2, Relief_Normal);
   Put (Window1.Fixed1, Window1.Button2, 72, 272);

   Gtk_New (Window1.Button3, -"button3");
   Set_Relief (Window1.Button3, Relief_Normal);
   Put (Window1.Fixed1, Window1.Button3, 136, 272);

   Gtk_New (Window1.Scrolledwindow1);
   Set_Policy (Window1.Scrolledwindow1, Policy_Always, Policy_Always);
   Put (Window1.Fixed1, Window1.Scrolledwindow1, 0, 0);

   Gtk_New (Window1.Textview1);
   Set_Editable (Window1.Textview1, True);
   Set_Justification (Window1.Textview1, Justify_Left);
   Set_Wrap_Mode (Window1.Textview1, Wrap_Word);
   Set_Cursor_Visible (Window1.Textview1, True);
   Set_Pixels_Above_Lines (Window1.Textview1, 0);
   Set_Pixels_Below_Lines (Window1.Textview1, 0);
   Set_Pixels_Inside_Wrap (Window1.Textview1, 0);
   Set_Left_Margin (Window1.Textview1, 0);
   Set_Right_Margin (Window1.Textview1, 0);
   Set_Indent (Window1.Textview1, 0);
   declare
      Iter : Gtk_Text_Iter;
   begin
      Get_Iter_At_Line (Get_Buffer (Window1.Textview1), Iter, 0);
      Insert (Get_Buffer (Window1.Textview1), Iter,
         -(""));
   end;
   Add (Window1.Scrolledwindow1, Window1.Textview1);

end Initialize;

end Window1_Pkg;

