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
with Glib.Convert; use Glib.Convert;
with Gtk; use Gtk;
with Gdk.Types; use Gdk.Types;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Enums; use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Tokeneer_Gu_I; use Callbacks_Tokeneer_Gu_I;
with Tokeneer_Gu_I_Intl; use Tokeneer_Gu_I_Intl;
with Load_Card_Pkg.Callbacks; use Load_Card_Pkg.Callbacks;
with Message_Box_Pkg;
with Ada.Strings.Fixed;
with Enclave_Pkg;
with TcpIp;

package body Load_Card_Pkg is


procedure Gtk_New (Load_Card : out Load_Card_Access) is
begin
   Load_Card := new Load_Card_Record;
   Load_Card_Pkg.Initialize (Load_Card);
end Gtk_New;

procedure Initialize (Load_Card : access Load_Card_Record'Class) is
   pragma Suppress (All_Checks);
begin
   Gtk.File_Selection.Initialize (Load_Card, -"Select Card");
   Set_Show_File_Op_Buttons (Load_Card, False);
   Set_Border_Width (Load_Card, 10);
   Set_Title (Load_Card, -"Select Card");
   Set_Position (Load_Card, Win_Pos_Center);
   Set_Modal (Load_Card, True);

   -- Glade/GATE doesn't want to insert the following:
   Button_Callback.Connect
      (Get_Cancel_Button(Load_Card), "clicked",
       Button_Callback.To_Marshaller (On_Cancelbutton_Clicked'Access), False);

   Button_Callback.Connect
      (Get_Ok_Button(Load_Card), "clicked",
       Button_Callback.To_Marshaller (On_OkButton_Clicked'Access), False);

end Initialize;

procedure Select_Card is
begin
   if Load_Card_Dlg = null then
      Gtk_New(Load_Card_Dlg);
   end if;

   -- Initialise the dialog:
   Set_Filename(Load_Card_Dlg, Enclave_Pkg.Default_Dir); -- default directory
   Complete(Load_Card_Dlg, "*.dat");                 -- default completion pattern

   Show_All(Load_Card_Dlg);
   Present(Load_Card_Dlg);
end Select_Card;

procedure Upload ( CardFile : in String;
                   CardDir  : in String)
is
   NewCard  : CardT := NullCardT;
   CardHandle : CommonTypes.Unsigned32T := 0;
   ValStart : Integer;
   Success  : Boolean := false;
   FingerPrint : BioAPI.IDT;
   TheCommand : TcpIp.MessageT;
begin
   -- Trim .dat from the filename if necessary:
   ValStart := Ada.Strings.Fixed.Index(Source  => CardFile,
                                       Pattern => ".dat");
   if ValStart > 1 then
      declare
         NewStr : String := Ada.Strings.Fixed.Head(Source => CardFile, Count => ValStart -1);
      begin
         NewCard.Card_Name(1..NewStr'Length) := NewStr;
      end;
   end if;

   if NextFree > MaxCards then
      Message_Box_Pkg.Show_Message("Card database already full");
   else
      begin
         -- Currently no mechanism for deleting cards from the server, so
         -- we simply grab the next slot rather than trying to search for a
         -- free slot:
         NewCard.Card_Handle := CommonTypes.Unsigned32T(NextFree -1);
         NewCard.Valid := True;
         CardDB(NextFree) := NewCard;

         -- Try to upload the card:
         SPRE_Interface.Create_Card_Message( CardDir => CardDir,
                                             CardName => CardFile,
                                             CardHandle => CardDB(NextFree).Card_Handle,
                                             FingerPrint => FingerPrint,
                                             CardCommand => TheCommand,
                                             Success => Success);
         CardDB(NextFree).Finger_Print := FingerPrint;

         if Success then
            SPRE_Interface.TisState.SendCommand( TheCommand => TheCommand,
                                                 Success => Success);

            if not Success then
               Message_Box_Pkg.Show_Message(
                  Locale_to_UTF8("Error uploading card " & CardFile));
               CardDB(NextFree).Valid := False;
            else
               NextFree := NextFree + 1;
            end if;
         else
            Message_Box_Pkg.Show_Message(CardFile & " does not appear to be a valid card definition");
         end if;

      exception
         when others =>
            Message_Box_Pkg.Show_Message(
               Locale_to_UTF8("Error uploading card " & CardFile));
      end;
   end if;

end Upload;

end Load_Card_Pkg;
