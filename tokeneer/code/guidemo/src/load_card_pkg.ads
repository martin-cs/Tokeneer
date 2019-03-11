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


with Gtk.File_Selection; use Gtk.File_Selection;
with Gtk.Button; use Gtk.Button;
with CommonTypes;
with CardSim;
with BioAPI;
with SPRE_Interface;

package Load_Card_Pkg is

   type Load_Card_Record is new Gtk_File_Selection_Record with record
      null;
   end record;
   type Load_Card_Access is access Load_Card_Record'Class;

   type CardT is record
      Card_Name    : SPRE_Interface.StringT;
      Card_Handle  : CommonTypes.Unsigned32T;
      Finger_Print : BioAPI.IDT;
      Valid        : Boolean;
   end record;

   NullCardT : CardT := (Card_Name    => (others => ASCII.nul),
                         Card_Handle  => 0,
                         Finger_Print => (others => ASCII.nul),
                         Valid        => False);

   MaxCards : constant := 100;
   type CardDB_T is array(1..MaxCards) of CardT;

   NextFree : Integer := 1;
   CardDB : CardDB_T := (others => NullCardT);

   Load_Card_Dlg : Load_Card_Access := null;

   procedure Gtk_New (Load_Card : out Load_Card_Access);
   procedure Initialize (Load_Card : access Load_Card_Record'Class);

   procedure Select_Card;

   procedure Upload ( CardFile : in String;
                      CardDir  : in String);

end Load_Card_Pkg;
