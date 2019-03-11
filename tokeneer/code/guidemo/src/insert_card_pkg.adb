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
with Gtk.Tree_Selection; use GTK.Tree_Selection;
with Gtk.Tree_Store; use GTK.Tree_Store;
with GTK.Tree_Model; use GTK.Tree_Model;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gdk.Types; use Gdk.Types;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Enums; use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Tokeneer_Gu_I; use Callbacks_Tokeneer_Gu_I;
with Tokeneer_Gu_I_Intl; use Tokeneer_Gu_I_Intl;
with Insert_Card_Pkg.Callbacks; use Insert_Card_Pkg.Callbacks;
with Load_Card_Pkg;
with Message_Box_Pkg;
with CommonTypes;

package body Insert_Card_Pkg is

   Insert_Card_Dlg : Insert_Card_Access := null;
   Card_List : Gtk_Tree_Store := null;

procedure Gtk_New (Insert_Card : out Insert_Card_Access) is
begin
   Insert_Card := new Insert_Card_Record;
   Insert_Card_Pkg.Initialize (Insert_Card);
end Gtk_New;

procedure Initialize (Insert_Card : access Insert_Card_Record'Class) is
   pragma Suppress (All_Checks);

   Col      : Gtk_Tree_View_Column;
   Num      : Gint;
   Text_Render   : Gtk_Cell_Renderer_Text;

begin
   Gtk.Dialog.Initialize (Insert_Card);
   Set_Title (Insert_Card, -"Select Token Card to Insert");
   Set_Position (Insert_Card, Win_Pos_Center);
   Set_Modal (Insert_Card, True);
   Set_Default_Size (Insert_Card, 200, 200);

   Gtk_New_From_Stock (Insert_Card.Cancelbutton1, "gtk-cancel");
   Set_Relief (Insert_Card.Cancelbutton1, Relief_Normal);
   Set_Flags (Insert_Card.Cancelbutton1, Can_Default);
   Button_Callback.Connect
     (Insert_Card.Cancelbutton1, "clicked",
      Button_Callback.To_Marshaller (On_Cancelbutton1_Clicked'Access), False);
   Pack_Start (Get_Action_Area (Insert_Card), Insert_Card.Cancelbutton1);

   Gtk_New_From_Stock (Insert_Card.Okbutton1, "gtk-ok");
   Set_Relief (Insert_Card.Okbutton1, Relief_Normal);
   Set_Flags (Insert_Card.Okbutton1, Can_Default);
   Button_Callback.Connect
     (Insert_Card.Okbutton1, "clicked",
      Button_Callback.To_Marshaller (On_Okbutton1_Clicked'Access), False);
   Pack_Start (Get_Action_Area (Insert_Card), Insert_Card.Okbutton1);

   Gtk_New (Insert_Card.Scrolledwindow3);
   Set_Policy (Insert_Card.Scrolledwindow3, Policy_Never, Policy_Automatic);
   Pack_Start
     (Get_Vbox (Insert_Card),
      Insert_Card.Scrolledwindow3,
      Expand  => True,
      Fill    => True,
      Padding => 0);

   Gtk_New(Card_List,
            (Card_Column     => GType_String,
             Bio_Column      => GType_String,
             Card_Handle_Col => GType_String,
             Edit_Col        => GType_Boolean,
             Foreground_Col  => GType_String));


   Gtk_New (Insert_Card.Treeview1, Card_List);

   Gtk_New (Text_Render);

   Gtk_New (Col);
   Num := Append_Column (Insert_Card.Treeview1, Col);
   Set_Title (Col, "Card Name");
   Pack_Start (Col, Text_Render, True);
   Add_Attribute (Col, Text_Render, "text", Card_Column);
   Add_Attribute (Col, Text_Render, "editable", Edit_Col);
   Add_Attribute (Col, Text_Render, "foreground", Foreground_Col);

   Gtk_New (Col);
   Num := Append_Column (Insert_Card.Treeview1, Col);
   Set_Title (Col, "Biometric Data");
   Pack_Start (Col, Text_Render, True);
   Add_Attribute (Col, Text_Render, "text", Bio_Column);
   Add_Attribute (Col, Text_Render, "editable", Edit_Col);
   Add_Attribute (Col, Text_Render, "foreground", Foreground_Col);

   Gtk_New (Col);
   Num := Append_Column (Insert_Card.Treeview1, Col);
   Set_Title (Col, "Card ID");
   Pack_Start (Col, Text_Render, True);
   Add_Attribute (Col, Text_Render, "text", Card_Handle_Col);
   Add_Attribute (Col, Text_Render, "editable", Edit_Col);
   Add_Attribute (Col, Text_Render, "foreground", Foreground_Col);

   Set_Headers_Visible (Insert_Card.Treeview1, True);
   Set_Rules_Hint (Insert_Card.Treeview1, False);
   Set_Reorderable (Insert_Card.Treeview1, False);
   Set_Enable_Search (Insert_Card.Treeview1, True);
   Add (Insert_Card.Scrolledwindow3, Insert_Card.Treeview1);

end Initialize;

procedure Populate_List
is
   Parent, Iter : GTK_Tree_Iter;
begin
   Clear(Card_List);
   Parent := Null_Iter;
   Set_Mode(Get_Selection(Insert_Card_Dlg.Treeview1), Selection_Single);

   for Count in 1 .. Load_Card_Pkg.MaxCards loop

      if Load_Card_Pkg.CardDB(Count).Valid then
         Append(Card_List, Iter, Parent);
         Set(Card_List, Iter, Card_Column, Locale_To_UTF8(Load_Card_Pkg.CardDB(Count).Card_Name));
         Set(Card_List, Iter, Bio_Column, Locale_To_UTF8(Load_Card_Pkg.CardDB(Count).Finger_Print));
         Set(Card_List, Iter, Card_Handle_Col, CommonTypes.Unsigned32T'Image(Load_Card_Pkg.CardDB(Count).Card_Handle));
         Set(Card_List, Iter, Foreground_Col, "black");
         Set(Card_List, Iter, Edit_Col, false);
      end if;

   end loop;
end Populate_List;

procedure Insert_Card( Reader : in ReaderNameT)
is
begin
   if Load_Card_Pkg.NextFree = 1 then
      Message_Box_Pkg.Show_Message("No cards have been loaded to the database!");
   else
      if Insert_Card_Dlg = null then
         Gtk_New(Insert_Card_Dlg);
      end if;

      Insert_Card_Dlg.Reader := Reader;
      Insert_Card_Dlg.Reader_Or_Bio := Token_Reader;

      Set_Title (Insert_Card_Dlg, -"Select Token Card to Insert");

      Populate_List;

      Show_All (Insert_Card_Dlg);
      Present (Insert_Card_Dlg);
   end if;
end Insert_Card;

procedure Remove_Card( Reader : in ReaderNameT)
is
begin
   SPRE_Interface.TisState.Remove_Token (Reader => Reader);
end Remove_Card;

procedure Present_Finger
is
begin
   if Load_Card_Pkg.NextFree = 1 then
      Message_Box_Pkg.Show_Message("No cards have been loaded to the database!");
   else
      if Insert_Card_Dlg = null then
         Gtk_New(Insert_Card_Dlg);
      end if;
      Set_Title (Insert_Card_Dlg, -"Select Biometric info to present");

      Insert_Card_Dlg.Reader_Or_Bio := Biometric_Device;

      Populate_List;

      Show_All (Insert_Card_Dlg);
      Present (Insert_Card_Dlg);
   end if;
end Present_Finger;

end Insert_Card_Pkg;
