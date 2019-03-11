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


with Glib.Error;
with Glib; use Glib;
with Glib.Convert; use Glib.Convert;
with Gtk; use Gtk;
with Gdk.Types; use Gdk.Types;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Enums; use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Tokeneer_Gu_I; use Callbacks_Tokeneer_Gu_I;
with Tokeneer_Gu_I_Intl; use Tokeneer_Gu_I_Intl;
with Enclave_Pkg.Callbacks; use Enclave_Pkg.Callbacks;
with Debug_Window_Pkg; use Debug_Window_Pkg;
with Message_Box_Pkg; use Message_Box_Pkg;
with Load_Card_Pkg;
with GDK.Pixbuf;
with Ada.Strings.Fixed;
with BioApi;
with TokenAPI;

package body Enclave_Pkg is

   Connected_to_Sim : Boolean := false;
   Light_Green_Image, Dark_Green_Image,
      Light_Red_Image, Dark_Red_Image: GDK.PixBuf.GDK_PixBuf;
   Alarm_On, Alarm_Light_On: Boolean := False;

procedure Initialise_Images is
   JunkError : Glib.Error.GError;
begin
   -- Load the LED pictures from file, if these fail then a 'broken link'
   -- image is used instead.
   Gdk.Pixbuf.Gdk_New_From_File
      (Pixbuf => Light_Red_Image,    Filename => Default_Dir & "light_red.png",   Error => JunkError);
   Gdk.Pixbuf.Gdk_New_From_File
      (Pixbuf => Light_Green_Image,  Filename => Default_Dir & "light_green.png", Error => JunkError);
   Gdk.Pixbuf.Gdk_New_From_File
      (Pixbuf => Dark_Red_Image,   Filename => Default_Dir & "dark_red.png",    Error => JunkError);
   Gdk.Pixbuf.Gdk_New_From_File
      (Pixbuf => Dark_Green_Image, Filename => Default_Dir & "dark_green.png",  Error => JunkError);
end Initialise_Images;

procedure Gtk_New (Enclave : out Enclave_Access) is
begin
   Enclave := new Enclave_Record;
   Enclave_Pkg.Initialize (Enclave);
   Enclave_Window := Enclave;
end Gtk_New;

procedure Initialize (Enclave : access Enclave_Record'Class) is
   pragma Suppress (All_Checks);
   Tooltips : Gtk_Tooltips;

begin
   Gtk.Window.Initialize (Enclave, Window_Toplevel);
   Set_Title (Enclave, -"TIS Enclave GUI");
   Set_Position (Enclave, Win_Pos_None);
   Set_Modal (Enclave, False);
   Return_Callback.Connect
     (Enclave, "delete_event", On_Enclave_Delete_Event'Access, False);

   Gtk_New_Vbox (Enclave.Vbox3, False, 0);
   Add (Enclave, Enclave.Vbox3);

   Gtk_New (Enclave.Menubar1);
   Pack_Start
     (Enclave.Vbox3,
      Enclave.Menubar1,
      Expand  => False,
      Fill    => False,
      Padding => 0);

   Gtk_New_With_Mnemonic (Enclave.Enclave_Menu, -("_Enclave"));
   Append (Enclave.Menubar1, Enclave.Enclave_Menu);

   Gtk_New (Enclave.Enclave_Menu_Menu);
   Set_Submenu (Enclave.Enclave_Menu, Enclave.Enclave_Menu_Menu);

   Gtk_New_With_Mnemonic (Enclave.Add_Card_To_Db, -("Add Card To DB"));
   Gtk_New (Tooltips);
   Set_Tip (Tooltips, Enclave.Add_Card_To_Db, -"Upload a card to the Simulator Card Database");
   Menu_Item_Callback.Connect
     (Enclave.Add_Card_To_Db, "activate",
      Menu_Item_Callback.To_Marshaller (On_Add_Card_To_Db_Activate'Access), False);
   Append (Enclave.Enclave_Menu_Menu, Enclave.Add_Card_To_Db);

   Gtk_New_From_Stock (Enclave.Debug_Window_Menu, "Show Debug Window");
   Image_Menu_Item_Callback.Connect
     (Enclave.Debug_Window_Menu, "activate",
      Image_Menu_Item_Callback.To_Marshaller (On_Debug_Window_Menu_Activate'Access), False);
   Append (Enclave.Enclave_Menu_Menu, Enclave.Debug_Window_Menu);


   Gtk_New_From_Stock (Enclave.Quit1, "gtk-quit");
   Image_Menu_Item_Callback.Connect
     (Enclave.Quit1, "activate",
      Image_Menu_Item_Callback.To_Marshaller (On_Quit1_Activate'Access), False);
   Append (Enclave.Enclave_Menu_Menu, Enclave.Quit1);

   Gtk_New_Hbox (Enclave.Hbox2, False, 0);
   Pack_Start
     (Enclave.Vbox3,
      Enclave.Hbox2,
      Expand  => False,
      Fill    => True,
      Padding => 0);

   Gtk_New_Vbox (Enclave.Vbox4, False, 0);
   Pack_Start
     (Enclave.Hbox2,
      Enclave.Vbox4,
      Expand  => True,
      Fill    => True,
      Padding => 0);

   Gtk_New (Enclave.Enclave_Frame);
   Set_Border_Width (Enclave.Enclave_Frame, 3);
   Set_Label_Align (Enclave.Enclave_Frame, 0.0, 0.5);
   Set_Shadow_Type (Enclave.Enclave_Frame, Shadow_Etched_In);
   Pack_Start
     (Enclave.Vbox4,
      Enclave.Enclave_Frame,
      Expand  => False,
      Fill    => True,
      Padding => 0);

   Gtk_New (Enclave.Table2, 2, 2, True);
   Set_Border_Width (Enclave.Table2, 5);
   Set_Row_Spacings (Enclave.Table2, 2);
   Set_Col_Spacings (Enclave.Table2, 10);
   Add (Enclave.Enclave_Frame, Enclave.Table2);

   Gtk_New (Enclave.Tis_Token_Reader, -"Insert Card");
   Set_Relief (Enclave.Tis_Token_Reader, Relief_Normal);
   Attach
     (Enclave.Table2,
       Enclave.Tis_Token_Reader,      Left_Attach  => 1,
      Right_Attach  => 2,
      Top_Attach  => 0,
      Bottom_Attach  => 1,
      Xoptions  => Fill,
      Xpadding  => 0,
      Ypadding  => 0);
   Button_Callback.Connect
     (Enclave.Tis_Token_Reader, "clicked",
      Button_Callback.To_Marshaller (On_Tis_Token_Reader_Clicked'Access), False);

   Gtk_New (Enclave.Label5, -("TIS Admin Console" & ASCII.LF
& "Token Reader"));
   Set_Alignment (Enclave.Label5, 0.0, 0.5);
   Set_Padding (Enclave.Label5, 0, 0);
   Set_Justify (Enclave.Label5, Justify_Left);
   Set_Line_Wrap (Enclave.Label5, False);
   Set_Selectable (Enclave.Label5, False);
   Set_Use_Markup (Enclave.Label5, False);
   Set_Use_Underline (Enclave.Label5, False);
   Attach
     (Enclave.Table2,
       Enclave.Label5,      Left_Attach  => 0,
      Right_Attach  => 1,
      Top_Attach  => 0,
      Bottom_Attach  => 1,
      Xoptions  => Fill,
      Xpadding  => 0,
      Ypadding  => 0);

   Gtk_New (Enclave.Label6, -("Alarm"));
   Set_Alignment (Enclave.Label6, 0.0, 0.5);
   Set_Padding (Enclave.Label6, 0, 0);
   Set_Justify (Enclave.Label6, Justify_Right);
   Set_Line_Wrap (Enclave.Label6, False);
   Set_Selectable (Enclave.Label6, False);
   Set_Use_Markup (Enclave.Label6, False);
   Set_Use_Underline (Enclave.Label6, False);
   Attach
     (Enclave.Table2,
       Enclave.Label6,      Left_Attach  => 0,
      Right_Attach  => 1,
      Top_Attach  => 1,
      Bottom_Attach  => 2,
      Xoptions  => Fill,
      Xpadding  => 0,
      Ypadding  => 0);

   Initialise_Images;

   Gtk_New (Enclave.Alarm , Dark_Red_Image);
   Set_Alignment (Enclave.Alarm, 0.5, 0.5);
   Set_Padding (Enclave.Alarm, 0, 0);
   Attach
     (Enclave.Table2,
       Enclave.Alarm,      Left_Attach  => 1,
      Right_Attach  => 2,
      Top_Attach  => 1,
      Bottom_Attach  => 2,
      Xoptions  => Fill,
      Yoptions  => Fill,
      Xpadding  => 0,
      Ypadding  => 0);

   Gtk_New (Enclave.Label1, -("Enclave"));
   Set_Alignment (Enclave.Label1, 0.5, 0.5);
   Set_Padding (Enclave.Label1, 0, 0);
   Set_Justify (Enclave.Label1, Justify_Left);
   Set_Line_Wrap (Enclave.Label1, False);
   Set_Selectable (Enclave.Label1, False);
   Set_Use_Markup (Enclave.Label1, False);
   Set_Use_Underline (Enclave.Label1, False);
   Set_Label_Widget (Enclave.Enclave_Frame,Enclave.Label1);

   Gtk_New (Enclave.Door_Frame);
   Set_Border_Width (Enclave.Door_Frame, 3);
   Set_Label_Align (Enclave.Door_Frame, 0.0, 0.5);
   Set_Shadow_Type (Enclave.Door_Frame, Shadow_Etched_In);
   Pack_Start
     (Enclave.Vbox4,
      Enclave.Door_Frame,
      Expand  => False,
      Fill    => True,
      Padding => 0);

   Gtk_New (Enclave.Table5, 2, 3, True);
   Set_Border_Width (Enclave.Table5, 5);
   Set_Row_Spacings (Enclave.Table5, 2);
   Set_Col_Spacings (Enclave.Table5, 0);
   Add (Enclave.Door_Frame, Enclave.Table5);

   Gtk_New (Enclave.Door_Button, -"Open Door");
   Set_Relief (Enclave.Door_Button, Relief_Normal);
   Attach
     (Enclave.Table5,
       Enclave.Door_Button,      Left_Attach  => 1,
      Right_Attach  => 2,
      Top_Attach  => 1,
      Bottom_Attach  => 2,
      Xoptions  => Fill,
      Xpadding  => 0,
      Ypadding  => 0);
   Button_Callback.Connect
     (Enclave.Door_Button, "clicked",
      Button_Callback.To_Marshaller (On_Door_Action_Button_Clicked'Access), False);

   Gtk_New (Enclave.Latch , Light_Red_Image);
   Set_Alignment (Enclave.Latch, 0.5, 0.5);
   Set_Padding (Enclave.Latch, 0, 0);
   Attach
     (Enclave.Table5,
       Enclave.Latch,      Left_Attach  => 1,
      Right_Attach  => 2,
      Top_Attach  => 0,
      Bottom_Attach  => 1,
      Xoptions  => Fill,
      Xpadding  => 0,
      Ypadding  => 0);

   Gtk_New (Enclave.Label11);
   Set_Alignment (Enclave.Label11, 0.0, 0.5);
   Set_Padding (Enclave.Label11, 0, 0);
   Set_Justify (Enclave.Label11, Justify_Left);
   Set_Line_Wrap (Enclave.Label11, False);
   Set_Selectable (Enclave.Label11, False);
   Set_Use_Markup (Enclave.Label11, False);
   Set_Use_Underline (Enclave.Label11, False);
   Attach
     (Enclave.Table5,
       Enclave.Label11,      Left_Attach  => 0,
      Right_Attach  => 1,
      Top_Attach  => 0,
      Bottom_Attach  => 1,
      Xpadding  => 0,
      Ypadding  => 0);

   Gtk_New (Enclave.Label12);
   Set_Alignment (Enclave.Label12, 0.0, 0.5);
   Set_Padding (Enclave.Label12, 0, 0);
   Set_Justify (Enclave.Label12, Justify_Left);
   Set_Line_Wrap (Enclave.Label12, False);
   Set_Selectable (Enclave.Label12, False);
   Set_Use_Markup (Enclave.Label12, False);
   Set_Use_Underline (Enclave.Label12, False);
   Attach
     (Enclave.Table5,
       Enclave.Label12,      Left_Attach  => 2,
      Right_Attach  => 3,
      Top_Attach  => 0,
      Bottom_Attach  => 1,
      Xpadding  => 0,
      Ypadding  => 0);

   Gtk_New (Enclave.Label2, -("Door"));
   Set_Alignment (Enclave.Label2, 0.5, 0.5);
   Set_Padding (Enclave.Label2, 0, 0);
   Set_Justify (Enclave.Label2, Justify_Left);
   Set_Line_Wrap (Enclave.Label2, False);
   Set_Selectable (Enclave.Label2, False);
   Set_Use_Markup (Enclave.Label2, False);
   Set_Use_Underline (Enclave.Label2, False);
   Set_Label_Widget (Enclave.Door_Frame,Enclave.Label2);

   Gtk_New (Enclave.Access_Frame);
   Set_Border_Width (Enclave.Access_Frame, 3);
   Set_Label_Align (Enclave.Access_Frame, 0.0, 0.5);
   Set_Shadow_Type (Enclave.Access_Frame, Shadow_Etched_In);
   Pack_Start
     (Enclave.Hbox2,
      Enclave.Access_Frame,
      Expand  => False,
      Fill    => False,
      Padding => 0);

   Gtk_New_Vbox (Enclave.Vbox5, False, 0);
   Set_Border_Width (Enclave.Vbox5, 5);
   Add (Enclave.Access_Frame, Enclave.Vbox5);

   Gtk_New (Enclave.Top_String);
   Set_Editable (Enclave.Top_String, False);
   Set_Max_Length (Enclave.Top_String, 0);
   Set_Text (Enclave.Top_String, -(""));
   Set_Visibility (Enclave.Top_String, True);
   Set_Invisible_Char (Enclave.Top_String, UTF8_Get_Char ("*"));
   Pack_Start
     (Enclave.Vbox5,
      Enclave.Top_String,
      Expand  => False,
      Fill    => False,
      Padding => 0);

   Gtk_New (Enclave.Bottom_String);
   Set_Editable (Enclave.Bottom_String, False);
   Set_Max_Length (Enclave.Bottom_String, 0);
   Set_Text (Enclave.Bottom_String, -(""));
   Set_Visibility (Enclave.Bottom_String, True);
   Set_Invisible_Char (Enclave.Bottom_String, UTF8_Get_Char ("*"));
   Pack_Start
     (Enclave.Vbox5,
      Enclave.Bottom_String,
      Expand  => False,
      Fill    => False,
      Padding => 0);

   Gtk_New (Enclave.Label10);
   Set_Alignment (Enclave.Label10, 0.5, 0.5);
   Set_Padding (Enclave.Label10, 0, 0);
   Set_Justify (Enclave.Label10, Justify_Left);
   Set_Line_Wrap (Enclave.Label10, False);
   Set_Selectable (Enclave.Label10, False);
   Set_Use_Markup (Enclave.Label10, False);
   Set_Use_Underline (Enclave.Label10, False);
   Pack_Start
     (Enclave.Vbox5,
      Enclave.Label10,
      Expand  => True,
      Fill    => True,
      Padding => 0);

   Gtk_New (Enclave.Table4, 2, 2, False);
   Set_Border_Width (Enclave.Table4, 5);
   Set_Row_Spacings (Enclave.Table4, 5);
   Set_Col_Spacings (Enclave.Table4, 10);
   Pack_Start
     (Enclave.Vbox5,
      Enclave.Table4,
      Expand  => False,
      Fill    => True,
      Padding => 0);

   Gtk_New (Enclave.Access_Token_Reader, -"Insert Card");
   Set_Relief (Enclave.Access_Token_Reader, Relief_Normal);
   Attach
     (Enclave.Table4,
       Enclave.Access_Token_Reader,      Left_Attach  => 1,
      Right_Attach  => 2,
      Top_Attach  => 0,
      Bottom_Attach  => 1,
      Xoptions  => Fill,
      Xpadding  => 0,
      Ypadding  => 0);
   Button_Callback.Connect
     (Enclave.Access_Token_Reader, "clicked",
      Button_Callback.To_Marshaller (On_Access_Token_Reader_Clicked'Access), False);

   Gtk_New (Enclave.Fingerprint, -"Present Fingerprint");
   Set_Relief (Enclave.Fingerprint, Relief_Normal);
   Attach
     (Enclave.Table4,
       Enclave.Fingerprint,      Left_Attach  => 1,
      Right_Attach  => 2,
      Top_Attach  => 1,
      Bottom_Attach  => 2,
      Xoptions  => Fill,
      Xpadding  => 0,
      Ypadding  => 0);
   Button_Callback.Connect
     (Enclave.Fingerprint, "clicked",
      Button_Callback.To_Marshaller (On_Fingerprint_Clicked'Access), False);

   Gtk_New (Enclave.Label8, -("Access Panel Token Reader"));
   Set_Alignment (Enclave.Label8, 0.0, 0.5);
   Set_Padding (Enclave.Label8, 0, 0);
   Set_Justify (Enclave.Label8, Justify_Left);
   Set_Line_Wrap (Enclave.Label8, False);
   Set_Selectable (Enclave.Label8, False);
   Set_Use_Markup (Enclave.Label8, False);
   Set_Use_Underline (Enclave.Label8, False);
   Attach
     (Enclave.Table4,
       Enclave.Label8,      Left_Attach  => 0,
      Right_Attach  => 1,
      Top_Attach  => 0,
      Bottom_Attach  => 1,
      Xoptions  => Fill,
      Xpadding  => 0,
      Ypadding  => 0);

   Gtk_New (Enclave.Label9, -("Biometric Input Device"));
   Set_Alignment (Enclave.Label9, 0.0, 0.5);
   Set_Padding (Enclave.Label9, 0, 0);
   Set_Justify (Enclave.Label9, Justify_Left);
   Set_Line_Wrap (Enclave.Label9, False);
   Set_Selectable (Enclave.Label9, False);
   Set_Use_Markup (Enclave.Label9, False);
   Set_Use_Underline (Enclave.Label9, False);
   Attach
     (Enclave.Table4,
       Enclave.Label9,      Left_Attach  => 0,
      Right_Attach  => 1,
      Top_Attach  => 1,
      Bottom_Attach  => 2,
      Xoptions  => Fill,
      Xpadding  => 0,
      Ypadding  => 0);

   Gtk_New (Enclave.Label3, -("Access Panel"));
   Set_Alignment (Enclave.Label3, 0.5, 0.5);
   Set_Padding (Enclave.Label3, 0, 0);
   Set_Justify (Enclave.Label3, Justify_Left);
   Set_Line_Wrap (Enclave.Label3, False);
   Set_Selectable (Enclave.Label3, False);
   Set_Use_Markup (Enclave.Label3, False);
   Set_Use_Underline (Enclave.Label3, False);
   Set_Label_Widget (Enclave.Access_Frame,Enclave.Label3);

end Initialize;

procedure UpdateDebugWindow is
begin
   Debug_Window_Pkg.Append("DOORCLOSED:     "  & Boolean'Image(TisState.Door.Closed));
   Debug_Window_Pkg.Append("DOORLOCKED:     "  & Boolean'Image(TisState.Door.Locked));
   Debug_Window_Pkg.Append("DISPTOPSTRING:  '" & Locale_to_UTF8(TisState.Display.TopString(1..TisState.Display.TopStringLength)) & "'");
   Debug_Window_Pkg.Append("DISPTOPSCROLL:  "  & Boolean'Image(TisState.Display.TopScroll));
   Debug_Window_Pkg.Append("DISPTOPWIDTH:   "  & Ada.Strings.Fixed.Trim(Source => SPRE_Interface.IndexT'Image(TisState.Display.TopDisplayWidth),
                                                                     Side   => Ada.Strings.Both));
   Debug_Window_Pkg.Append("DISPBOTSTRING:  '" & Locale_to_UTF8(TisState.Display.BottomString(1..TisState.Display.BottomStringLength)) & "'");

   Debug_Window_Pkg.Append("DISPBOTWIDTH:   "  & Ada.Strings.Fixed.Trim(Source => SPRE_Interface.IndexT'Image(TisState.Display.BottomDisplayWidth),
                                                                     Side   => Ada.Strings.Both));
   Debug_Window_Pkg.Append("ALARMACTIVE:    "  & Boolean'Image(TisState.Alarm.Active));
   Debug_Window_Pkg.Append("BIONOIMAGE:     "  & Boolean'Image(TisState.Bio.NoImage));
   Debug_Window_Pkg.Append("BIOIMAGE:       '" & TisState.Bio.ImageTemplate(1..TisState.Bio.ImageLength) & "'");
   Debug_Window_Pkg.Append("BIOFAR:         "  & Ada.Strings.Fixed.Trim(Source => BioAPI.RateT'Image(TisState.Bio.FAR),
                                                                     Side   => Ada.Strings.Both));
   Debug_Window_Pkg.Append("BIOFRR:         "  & Ada.Strings.Fixed.Trim(Source => BioAPI.RateT'Image(TisState.Bio.FRR),
                                                                     Side   => Ada.Strings.Both));

   Debug_Window_Pkg.Append("TR1 NAME:       "  & Locale_to_UTF8(TisState.ExtReader.Reader_Name(1..TisState.ExtReader.Reader_Length)));
   Debug_Window_Pkg.Append("TR1 STATE:      "  & Ada.Strings.Fixed.Trim(
                                                   Source => TokenAPI.ReaderStateT'Image(TisState.ExtReader.Reader_State),
                                                   Side => Ada.Strings.Both));
   Debug_Window_Pkg.Append("TR1 CARD NAME:  "  & Locale_to_UTF8(TisState.ExtReader.Card_Handle(1..TisState.ExtReader.Card_Length)));
   Debug_Window_Pkg.Append("TR1 CARD STATE: "  & Ada.Strings.Fixed.Trim(
                                                   Source => TokenAPI.CardStateT'Image(TisState.ExtReader.Card_State),
                                                   Side => Ada.Strings.Both));

   Debug_Window_Pkg.Append("TR2 NAME:       "  & Locale_to_UTF8(TisState.IntReader.Reader_Name(1..TisState.IntReader.Reader_Length)));
   Debug_Window_Pkg.Append("TR2 STATE:      "  & Ada.Strings.Fixed.Trim(
                                                   Source => TokenAPI.ReaderStateT'Image(TisState.IntReader.Reader_State),
                                                   Side => Ada.Strings.Both));
   Debug_Window_Pkg.Append("TR2 CARD NAME:  "  & Locale_to_UTF8(TisState.IntReader.Card_Handle(1..TisState.IntReader.Card_Length)));
   Debug_Window_Pkg.Append("TR2 CARD STATE: "  & Ada.Strings.Fixed.Trim(
                                                   Source => TokenAPI.CardStateT'Image(TisState.IntReader.Card_State),
                                                   Side => Ada.Strings.Both));



   Debug_Window_Pkg.Append("");
end UpdateDebugWindow;

procedure UpdateEnclaveDisplay is
begin
   -- Handle alarm:
   -- Catch the alarm going inactive:
   if Alarm_On and not TisState.Alarm.Active then
      Set(Enclave_Window.Alarm, Dark_Red_Image);
   end if;
   Alarm_On := TisState.Alarm.Active;

   -- Handle door latch:
   if TisState.Door.Locked then
      Set(Enclave_Window.Latch, Light_Red_Image);
   else
      Set(Enclave_Window.Latch, Light_Green_Image);
   end if;

   -- Handle door open/close:
   if TisState.Door.Closed then
      Set_Label(Enclave_Window.Door_Button, "Open Door");
   else
      Set_Label(Enclave_Window.Door_Button, "Close Door");
   end if;

   -- Handle display strings.
   -- TODO: scrolling?
   Set_Text(Enclave_Window.Top_String, Locale_to_UTF8(TisState.Display.TopString(1..TisState.Display.TopStringLength)));
   Set_Text(Enclave_Window.Bottom_String, Locale_to_UTF8(TisState.Display.BottomString(1..TisState.Display.BottomStringLength)));

end UpdateEnclaveDisplay;

function TIS_Timer return Boolean is
   Call_Again: Boolean := false;
begin
   if Enclave_Window /= null then
      Spre_Interface.TisState.UpdateState;
      TISState := SPRE_Interface.TisState.CurrentState;

      if Debug_Window_Pkg.Dbg_Window /= null then
         UpdateDebugWindow;
      end if;

      UpdateEnclaveDisplay;

      Call_Again := True;
   end if;

   return Call_Again;

end TIS_Timer;

function Alarm_Timer_Func return boolean is
   Call_Again: Boolean := false;
begin
   if Enclave_Window /= null then
      Call_Again := true;
      if Alarm_On then
         if Alarm_Light_On then
            Set(Enclave_Window.Alarm, Dark_Red_Image);
         else
            Set(Enclave_Window.Alarm, Light_Red_Image);
         end if;

         Alarm_Light_On := not Alarm_Light_On;
      end if;
   end if;

   return Call_Again;
end Alarm_Timer_Func;

procedure Start_Enclave_Gui is
   Tmp_Enclave_Record: Enclave_Record;
   OK: Boolean;
begin
   if Enclave_Window = null then
      -- If we've not already connected to the Simulator, try to make the
      -- connection to the simulator: we ignore the success parameter
      if not Connected_to_Sim then
         Spre_Interface.InitTestDevices(OK);
         Connected_To_Sim := Ok;
      end if;

      if Connected_to_Sim then
         Gtk_New(Enclave_Window);
         Show_All(Enclave_Window);
         SPRE_Interface.TisState.UpdateState;
         TISState := SPRE_Interface.TisState.CurrentState;

         -- Pre-load a certain list of cards:
         Load_Card_Pkg.Upload( CardFile => "AdminLogin2_p06.dat", CardDir => Default_Dir);
         Load_Card_Pkg.Upload( CardFile => "AdminLogout1_p07.dat", CardDir => Default_Dir);
         Load_Card_Pkg.Upload( CardFile => "UserEntry1_p01.dat", CardDir => Default_Dir);
         Load_Card_Pkg.Upload( CardFile => "UserEntry13_p07.dat", CardDir => Default_Dir);
         Load_Card_Pkg.Upload( CardFile => "UserEntry2_p02.dat", CardDir => Default_Dir);
         Load_Card_Pkg.Upload( CardFile => "UserEntry4_p03.dat", CardDir => Default_Dir);
         Load_Card_Pkg.Upload( CardFile => "UserEntry5_p04.dat", CardDir => Default_Dir);
         Load_Card_Pkg.Upload( CardFile => "UserEntry6_p05.dat", CardDir => Default_Dir);

         -- Add the timeout to update the state:
         Timer := Gtk.Main.Timeout_Add(200, Tis_Timer'access);
         Alarm_Timer := Gtk.Main.Timeout_Add(500, Alarm_Timer_Func'access);
      else
         Message_Box_Pkg.Show_Message("Error trying to connect to simulator");
      end if;
   else
      Present(Enclave_Window);
   end if;
end Start_Enclave_Gui;

end Enclave_Pkg;





