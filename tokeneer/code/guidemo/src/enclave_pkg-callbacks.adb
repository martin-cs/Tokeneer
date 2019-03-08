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
with Debug_Window_Pkg; use Debug_Window_Pkg;
with SPRE_Interface;
with Gtk.Main;
with Door_Dialog_Pkg;
with Load_Card_Pkg;
with Insert_Card_Pkg;

package body Enclave_Pkg.Callbacks is

   use Gtk.Arguments;

   procedure CleanUp is
   begin
      -- Disconnect from the simulator, turn off the timeout-handler and destroy
      -- the debug window:
      SPRE_Interface.FinaliseTestDevices;
      Gtk.Main.Timeout_Remove(Enclave_Pkg.Timer);
      Gtk.Main.Timeout_Remove(Enclave_Pkg.Alarm_Timer);
      if Debug_Window_pkg.Dbg_Window /= null then
         Destroy(Debug_Window_Pkg.Dbg_Window);
      end if;
   end CleanUp;

   -----------------------------
   -- On_Enclave_Delete_Event --
   -----------------------------

   function On_Enclave_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      CleanUp;
      Destroy(Enclave_Pkg.Enclave_Window);
      Enclave_Pkg.Enclave_Window := null;
      return False;
   end On_Enclave_Delete_Event;

   --------------------------------
   -- On_Add_Card_To_Db_Activate --
   --------------------------------

   procedure On_Add_Card_To_Db_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      Load_Card_Pkg.Select_Card;
   end On_Add_Card_To_Db_Activate;

   -----------------------------------
   -- On_Debug_Window_Menu_Activate --
   -----------------------------------

   procedure On_Debug_Window_Menu_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class)
   is
      Tmp_Dbg_Rec : Debug_Window_Pkg.Debug_Window_Access;
   begin
      if Debug_Window_Pkg.Dbg_Window = null then
         Debug_Window_Pkg.Gtk_New(Tmp_Dbg_Rec);
         Show_All(Tmp_Dbg_Rec);
      else
         Present(Debug_Window_Pkg.Dbg_Window);
      end if;
   end On_Debug_Window_Menu_Activate;

   -----------------------
   -- On_Quit1_Activate --
   -----------------------

   procedure On_Quit1_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class)
   is
   begin
      CleanUp;
      Destroy(Enclave_Pkg.Enclave_Window);
      Enclave_Pkg.Enclave_Window := null;
   end On_Quit1_Activate;

   ---------------------------------
   -- On_Tis_Token_Reader_Clicked --
   ---------------------------------

   procedure On_Tis_Token_Reader_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      if Get_Label(Object) = "Insert Card" then
         Insert_Card_Pkg.Insert_Card("INTREAD ");
      else
         Insert_Card_Pkg.Remove_Card("INTREAD ");
         Set_Label(Object, "Insert Card");
      end if;
   end On_Tis_Token_Reader_Clicked;

   -----------------------------------
   -- On_Door_Action_Button_Clicked --
   -----------------------------------

   procedure On_Door_Action_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      if TisState.Door.Closed then
         if TisState.Door.Locked then
            Door_Dialog_Pkg.Ask;
         else
            SPRE_Interface.TisState.Open_Door;
         end if;
      else
         SPRE_Interface.TisState.Close_Door;
      end if;
   end On_Door_Action_Button_Clicked;

   ------------------------------------
   -- On_Access_Token_Reader_Clicked --
   ------------------------------------

   procedure On_Access_Token_Reader_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      if Get_Label(Object) = "Insert Card" then
         Insert_Card_Pkg.Insert_Card("EXTREAD ");
      else
         Insert_Card_Pkg.Remove_Card("EXTREAD ");
         Set_Label(Object, "Insert Card");
      end if;
   end On_Access_Token_Reader_Clicked;

   ----------------------------
   -- On_Fingerprint_Clicked --
   ----------------------------

   procedure On_Fingerprint_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Insert_Card_Pkg.Present_Finger;
   end On_Fingerprint_Clicked;

end Enclave_Pkg.Callbacks;
