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
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with SPRE_Interface;
with Message_Box_Pkg;
with CommonTypes;
with Enclave_Pkg;

package body Insert_Card_Pkg.Callbacks is

   use Gtk.Arguments;

   ------------------------------
   -- On_Cancelbutton1_Clicked --
   ------------------------------

   procedure On_Cancelbutton1_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Hide(Get_Toplevel(Object));
   end On_Cancelbutton1_Clicked;

   --------------------------
   -- On_Okbutton1_Clicked --
   --------------------------

   procedure On_Okbutton1_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      ICD : Insert_Card_Access;
      Iter : Gtk_Tree_Iter;
      M : Gtk_Tree_Model;
   begin
      ICD := Insert_Card_Access(Get_Toplevel(Object));
      Get_Selected(Get_Selection(ICD.Treeview1), M, Iter);

      if Iter = Null_Iter then
         Message_Box_Pkg.Show_Message("Please select an item from the list");
      else
         if ICD.Reader_or_Bio = Token_Reader then
            SPRE_Interface.TisState.Insert_Token(
               Reader => ICD.Reader,
               CardID => CommonTypes.Unsigned32T'Value(Get_String(M, Iter, Card_Handle_Col)));

            -- Toggle the labels:
            if ICD.Reader = "INTREAD "  then
               Set_Label(Enclave_Pkg.Enclave_Window.TIS_Token_Reader, "Remove Card");
            else
               Set_Label(Enclave_Pkg.Enclave_Window.Access_Token_Reader, "Remove Card");
            end if;
         else
            SPRE_Interface.TisState.Send_Fingerprint(
               Image   => Get_String(M, Iter, Bio_Column),
               FAR_Val => 0,
               FRR_Val => 0);
         end if;

         Hide(ICD);
      end if;
   end On_Okbutton1_Clicked;

end Insert_Card_Pkg.Callbacks;
