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
with Debug_Window_Pkg.Callbacks; use Debug_Window_Pkg.Callbacks;
with Ada.Characters.Latin_1;

package body Debug_Window_Pkg is

   CRLF : constant string := Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.LF;

procedure Gtk_New (Debug_Window : out Debug_Window_Access) is
begin
   Debug_Window := new Debug_Window_Record;
   Debug_Window_Pkg.Initialize (Debug_Window);
   Dbg_Window := Debug_Window;
end Gtk_New;

procedure Initialize (Debug_Window : access Debug_Window_Record'Class) is
   pragma Suppress (All_Checks);
begin
   Gtk.Window.Initialize (Debug_Window, Window_Toplevel);
   Set_Title (Debug_Window, -"Debug Window");
   Set_Position (Debug_Window, Win_Pos_None);
   Set_Modal (Debug_Window, False);
   Set_Default_Size (Debug_Window, 255, 400);
   Return_Callback.Connect
     (Debug_Window, "delete_event", On_Debugwindow_Delete_Event'Access, False);

   Gtk_New_Vbox (Debug_Window.Vbox1, False, 0);
   Add (Debug_Window, Debug_Window.Vbox1);

   Gtk_New (Debug_Window.Scrolledwindow2);
   Set_Policy (Debug_Window.Scrolledwindow2, Policy_Automatic, Policy_Always);
   Pack_Start
     (Debug_Window.Vbox1,
      Debug_Window.Scrolledwindow2,
      Expand  => True,
      Fill    => True,
      Padding => 0);

   Gtk_New (Debug_Window.Textview2);
   Set_Editable (Debug_Window.Textview2, True);
   Set_Justification (Debug_Window.Textview2, Justify_Left);
   Set_Wrap_Mode (Debug_Window.Textview2, Wrap_None);
   Set_Cursor_Visible (Debug_Window.Textview2, True);
   Set_Pixels_Above_Lines (Debug_Window.Textview2, 0);
   Set_Pixels_Below_Lines (Debug_Window.Textview2, 0);
   Set_Pixels_Inside_Wrap (Debug_Window.Textview2, 0);
   Set_Left_Margin (Debug_Window.Textview2, 0);
   Set_Right_Margin (Debug_Window.Textview2, 0);
   Set_Indent (Debug_Window.Textview2, 0);
   declare
      Iter : Gtk_Text_Iter;
   begin
      Get_Iter_At_Line (Get_Buffer (Debug_Window.Textview2), Iter, 0);
      Insert (Get_Buffer (Debug_Window.Textview2), Iter,
         -(""));
   end;
   Add (Debug_Window.Scrolledwindow2, Debug_Window.Textview2);

   Gtk_New (Debug_Window.Table1, 1, 3, False);
   Set_Row_Spacings (Debug_Window.Table1, 0);
   Set_Col_Spacings (Debug_Window.Table1, 0);
   Pack_End
     (Debug_Window.Vbox1,
      Debug_Window.Table1,
      Expand  => False,
      Fill    => False,
      Padding => 0);

   Gtk_New (Debug_Window.Close, -"Close");
   Set_Relief (Debug_Window.Close, Relief_Normal);
   Attach
     (Debug_Window.Table1,
       Debug_Window.Close,      Left_Attach  => 1,
      Right_Attach  => 2,
      Top_Attach  => 0,
      Bottom_Attach  => 1,
      Xoptions  => Fill,
      Xpadding  => 0,
      Ypadding  => 0);
   Button_Callback.Connect
     (Debug_Window.Close, "clicked",
      Button_Callback.To_Marshaller (On_Close_Clicked'Access), False);

   Gtk_New (Debug_Window.Pause, -"Pause");
   Set_Relief (Debug_Window.Pause, Relief_Normal);
   Set_Active (Debug_Window.Pause, False);
   Set_Inconsistent (Debug_Window.Pause, False);
   Set_Relief (Debug_Window.Pause, Relief_Normal);
   Set_Use_Underline (Debug_Window.Pause, True);
   Attach
     (Debug_Window.Table1,
       Debug_Window.Pause,      Left_Attach  => 2,
      Right_Attach  => 3,
      Top_Attach  => 0,
      Bottom_Attach  => 1,
      Xoptions  => Fill,
      Xpadding  => 0,
      Ypadding  => 0);

end Initialize;

procedure Append(Text : in UTF8_String)
is
   Iter : Gtk_Text_Iter;
   Dummy: Boolean;
begin
   if not Get_Active(Dbg_Window.Pause) and Dbg_Window /= null then
      Get_End_Iter (Get_Buffer (Dbg_Window.Textview2), Iter);
      Insert (Get_Buffer (Dbg_Window.Textview2), Iter, Text & CRLF);
      Dummy := Scroll_To_Iter(Dbg_Window.Textview2, Iter, 0.0, False, 0.0, 0.0);
   end if;
end;

end Debug_Window_Pkg;

