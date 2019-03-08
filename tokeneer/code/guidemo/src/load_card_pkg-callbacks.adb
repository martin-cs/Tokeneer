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
with Ada.Strings;
with Ada.Strings.Fixed;
with Message_Box_Pkg;

package body Load_Card_Pkg.Callbacks is

   use Gtk.Arguments;

   --------------------
   -- On_Cancelbutton_Clicked --
   --------------------

   procedure On_Cancelbutton_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Hide(Get_Toplevel(Object));
   end On_Cancelbutton_Clicked;

   -------------------
   -- On_Okbutton_Clicked --
   -------------------

   procedure On_Okbutton_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      CardFile : UTF8_String := Get_Filename(Load_Card_DLG);
      Last_Slash : Integer;
   begin
      Last_Slash := Ada.Strings.Fixed.Index(Source  => CardFile,
                                            Pattern => "\",
                                            Going => Ada.Strings.Backward);

      if Last_Slash = CardFile'Length then
         Message_Box_Pkg.Show_Message("Please select a file!");
      else
         Hide(Get_Toplevel(Object));

         -- Try to upload the card:
         Load_Card_Pkg.Upload(CardDir  => CardFile(1..Last_Slash),
                              CardFile => CardFile(Last_Slash +1 ..  CardFile'Length));
      end if;
   end On_Okbutton_Clicked;

end Load_Card_Pkg.Callbacks;
