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


with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Menu_Bar; use Gtk.Menu_Bar;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Menu; use Gtk.Menu;
with Gtk.Tooltips; use Gtk.Tooltips;
with Gtk.Image_Menu_Item; use Gtk.Image_Menu_Item;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Table; use Gtk.Table;
with Gtk.Button; use Gtk.Button;
with Gtk.Label; use Gtk.Label;
with Gtk.Image; use Gtk.Image;
with Gtk.GEntry; use Gtk.GEntry;
with Glib.Unicode; use Glib.Unicode;
with Gtk.Main;
with SPRE_Interface;

package Enclave_Pkg is

   Default_Dir : constant String := "C:\tokeneer\data\";

   type Enclave_Record is new Gtk_Window_Record with record
      Vbox3 : Gtk_Vbox;
      Menubar1 : Gtk_Menu_Bar;
      Enclave_Menu : Gtk_Menu_Item;
      Enclave_Menu_Menu : Gtk_Menu;
      Add_Card_To_Db : Gtk_Menu_Item;
      Debug_Window_Menu : Gtk_Image_Menu_Item;
      Quit1 : Gtk_Image_Menu_Item;
      Hbox2 : Gtk_Hbox;
      Vbox4 : Gtk_Vbox;
      Enclave_Frame : Gtk_Frame;
      Table2 : Gtk_Table;
      Tis_Token_Reader : Gtk_Button;
      Label5 : Gtk_Label;
      Label6 : Gtk_Label;
      Alarm : Gtk_Image;
      Label1 : Gtk_Label;
      Door_Frame : Gtk_Frame;
      Table5 : Gtk_Table;
      Door_Button : Gtk_Button;
      Latch : Gtk_Image;
      Label11 : Gtk_Label;
      Label12 : Gtk_Label;
      Label2 : Gtk_Label;
      Access_Frame : Gtk_Frame;
      Vbox5 : Gtk_Vbox;
      Top_String : Gtk_Entry;
      Bottom_String : Gtk_Entry;
      Label10 : Gtk_Label;
      Table4 : Gtk_Table;
      Access_Token_Reader : Gtk_Button;
      Fingerprint : Gtk_Button;
      Label8 : Gtk_Label;
      Label9 : Gtk_Label;
      Label3 : Gtk_Label;
   end record;
   type Enclave_Access is access Enclave_Record'Class;

   Enclave_Window: Enclave_Access := null;
   TISState : SPRE_Interface.TisStateT;
   Alarm_Timer, Timer : Gtk.Main.Timeout_Handler_ID;

   procedure Gtk_New (Enclave : out Enclave_Access);
   procedure Initialize (Enclave : access Enclave_Record'Class);

   procedure Start_Enclave_Gui;

end Enclave_Pkg;
