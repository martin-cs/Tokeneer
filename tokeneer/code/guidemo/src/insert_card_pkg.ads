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


with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Button; use Gtk.Button;
with Gtk.Object; use Gtk.Object;
with Gtk.Box; use Gtk.Box;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Tree_View; use Gtk.Tree_View;
with SPRE_Interface;

package Insert_Card_Pkg is
   type DeviceT is (Biometric_Device, Token_Reader);

   subtype ReaderNameT is String(1..8);

   type Insert_Card_Record is new Gtk_Dialog_Record with record
      Reader_or_Bio : DeviceT;
      Reader : ReaderNameT;
      Cancelbutton1 : Gtk_Button;
      Okbutton1 : Gtk_Button;
      Scrolledwindow3 : Gtk_Scrolled_Window;
      Treeview1 : Gtk_Tree_View;
   end record;
   type Insert_Card_Access is access all Insert_Card_Record'Class;

   Card_Column     : constant := 0;
   Bio_Column      : constant := 1;
   Card_Handle_Col : constant := 2;
   Edit_Col        : constant := 3;
   Foreground_Col  : constant := 4;

   procedure Gtk_New (Insert_Card : out Insert_Card_Access);
   procedure Initialize (Insert_Card : access Insert_Card_Record'Class);

   procedure Insert_Card( Reader : in ReaderNameT);
   procedure Remove_Card( Reader : in ReaderNameT);
   procedure Present_Finger;

end Insert_Card_Pkg;
