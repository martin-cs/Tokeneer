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
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Main;
with Gtk.Text; use Gtk.Text;

package body Text_Window_Pkg.Callbacks is

   use Gtk.Arguments;

   ------------------------
   -- On_Button3_Clicked --
   ------------------------

   procedure On_Button3_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      -- GTK.Main.Main_Quit;
      Destroy(Get_Toplevel(Object));
      Gtk.Main.Main_Quit;
   end On_Button3_Clicked;

   ------------------------
   -- On_Button1_Clicked --
   ------------------------

   procedure On_Button1_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      Iter : Gtk_Text_Iter;
   begin
      Get_Iter_At_Line (Get_Buffer (Main_Window.Textview1), Iter, 0);
      Insert (Get_Buffer (Main_Window.Textview1), Iter, "Help");
      -- Gtk.Text.Insert(Text => Get_Buffer(Main_Window.Textview1, Chars => "Hello World");
   end On_Button1_Clicked;

   ------------------------
   -- On_Button2_Clicked --
   ------------------------

   procedure On_Button2_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Button2_Clicked;

end Text_Window_Pkg.Callbacks;
