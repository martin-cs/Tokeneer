------------------------------------------------------------------
-- Copyright 2004 Praxis Critical Systems Ltd
-- Graphical User Interface demo
--
-- File:
--    $Id: spre_interface.adb,v 1.5 2004/04/09 21:28:49 mjw Exp $
--
------------------------------------------------------------------
with Ada.Streams.Stream_IO;
with Ada.Strings;
with File;

package body SPRE_Interface is

   protected body TisState is

      function CurrentState return TisStateT
      is
      begin
         return State;
      end CurrentState;


      function GetDisplayState return DisplayStateT
      is
         DisplayState : DisplayStateT;
         TheCommand   : TcpIp.MessageT;
         TheReturn    : TcpIp.MessageT;
         Success      : Boolean;
      begin
         TheCommand.Length := 14;
         TheCommand.Data(1..TheCommand.Length) := "display.read()";
         TcpIp.SendAndReceive ( IsAdmin  => False,
                                Outgoing => TheCommand,
                                Incoming => TheReturn,
                                Success  => Success);

         if Success then
            declare
               TopDict : MsgProc.DictionaryT :=
                  MsgProc.GetDictionaryByKey(
                     Dic => MsgProc.GetDictionary(Msg => TheReturn.Data, Arg => 1),
                     Key => "top");
               BotDict : MsgProc.DictionaryT :=
                  MsgProc.GetDictionaryByKey(
                     Dic => MsgProc.GetDictionary(Msg => TheReturn.Data, Arg => 1),
                     Key => "bottom");
               TopString : String := MsgProc.GetStringByKey(Dic => TopDict, Key => "text");
               BotString : String := MsgProc.GetStringByKey(Dic => BotDict, Key => "text");
            begin
               DisplayState.TopString := StringT'(others => ' ');
               DisplayState.TopString(1..TopString'Length) := TopString;
               DisplayState.TopStringLength := TopString'Length;
               DisplayState.TopScroll := Boolean'Value(MsgProc.GetStringByKey(Dic => TopDict, Key => "scroll?"));
               DisplayState.TopDisplayWidth := IndexT'Value (MsgProc.GetStringByKey(Dic => TopDict, Key => "len"));

               DisplayState.BottomString := StringT'(others => ' ');
               DisplayState.BottomString(1..BotString'Length) := BotString;
               DisplayState.BottomStringLength := BotString'Length;
               DisplayState.BottomDisplayWidth := IndexT'Value (MsgProc.GetStringByKey(Dic => BotDict, Key => "len"));
            end;
         else
            -- Arbitrary defaults
            DisplayState := DisplayStateT'(
               TopString          => StringT'(others => ' '),
               TopStringLength    => 1,
               TopScroll          => False,
               TopDisplayWidth    => 80,
               BottomString       => StringT'(others => ' '),
               BottomStringLength => 1,
               BottomDisplayWidth => 80);
         end if;

         return DisplayState;
      end GetDisplayState;


      function GetAlarmState return AlarmStateT
      is
         TheCommand : TcpIp.MessageT;
         TheReturn  : TcpIp.MessageT;
         Success    : Boolean;
         AlarmState : AlarmStateT;
      begin
         TheCommand.Length := 16;
         TheCommand.Data(1..TheCommand.Length) := "alarm.getState()";
         TcpIp.SendAndReceive ( IsAdmin  => False,
                                Outgoing => TheCommand,
                                Incoming => TheReturn,
                                Success  => Success);

         if Success then
            AlarmState.Active := Boolean'Value(MsgProc.GetStringByKey(
               Dic => MsgProc.GetDictionary(Msg => TheReturn.Data, Arg => 1),
               Key => "activated?"));
         else
            AlarmState.Active := False; -- Arbitrary
         end if;

         return AlarmState;
      end GetAlarmState;


      function GetDoorState return DoorStateT
      is
         TheCommand : TcpIp.MessageT;
         TheReturn  : TcpIp.MessageT;
         Success    : Boolean;
         DoorState  : DoorStateT;
      begin
         TheCommand.Length := 15;
         TheCommand.Data(1..TheCommand.Length) := "door.getState()";
         TcpIp.SendAndReceive ( IsAdmin  => False,
                                Outgoing => TheCommand,
                                Incoming => TheReturn,
                                Success  => Success);

         if Success then
            declare
               Dict : MsgProc.DictionaryT := MsgProc.GetDictionary(Msg => TheReturn.Data, Arg => 1);
            begin
               DoorState := DoorStateT'(
                  Closed => Boolean'Value(MsgProc.GetStringByKey(
                               Dic => Dict,
                               Key => "closed?")),
                  Locked => Boolean'Value(MsgProc.GetStringByKey(
                               Dic => Dict,
                               Key => "locked?")));
            end;
         else
            DoorState := DoorStateT'(
               Closed => True,
               Locked => True); -- Arbitrary
         end if;

         return DoorState;

      end GetDoorState;


      function GetBioState return BioStateT
      is
         TheCommand : TcpIp.MessageT;
         TheReturn  : TcpIp.MessageT;
         Success    : Boolean;
         BioState   : BioStateT;
      begin
         TheCommand.Length := 20;
         TheCommand.Data(1..TheCommand.Length) := "bioDevice.getState()";
         TcpIp.SendAndReceive ( IsAdmin  => False,
                                Outgoing => TheCommand,
                                Incoming => TheReturn,
                                Success  => Success);

         if Success then
            declare
               MainDict : MsgProc.DictionaryT := MsgProc.GetDictionary(
                  Msg => TheReturn.Data, Arg => 1);
               ImageDict : MsgProc.DictionaryT :=
                  MsgProc.GetDictionaryByKey(
                     Dic => MainDict,
                     Key => "image");
               ImageString : String := MsgProc.GetStringByKey(
                  Dic => ImageDict, Key => "imageTemplate");
            begin
               BioState.NoImage := Boolean'Value(MsgProc.GetStringByKey(
                                      Dic => MainDict,
                                      Key => "noImage?"));
               BioState.ImageLength := ImageString'Length;
               BioState.ImageTemplate := BioAPI.IDT'(others => ASCII.Nul);
               BioState.ImageTemplate(1..BioState.ImageLength) := ImageString;
               BioState.FAR := BioAPI.RateT'Value(MsgProc.GetStringByKey(
                                  Dic => MainDict,
                                  Key => "FAR"));
               BioState.FRR := BioAPI.RateT'Value(MsgProc.GetStringByKey(
                                  Dic => MainDict,
                                  Key => "FRR"));
            end;
         else
            -- Arbitrary default
            BioState := BioStateT'(
               NoImage       => True,
               ImageTemplate => BioAPI.IDT'(others => ASCII.Nul),
               ImageLength   => 0,
               FAR           => -1,
               FRR           => -1);
         end if;

         return BioState;
      end GetBioState;


      function GetReaderState(Reader: String) return TokenReaderStateT
      is
         ReaderState : TokenReaderStateT;
         TheCommand : TcpIp.MessageT;
         TheReturn  : TcpIp.MessageT;
         Success    : Boolean;
      begin
         TheCommand.Length := 24 + Reader'Length;
         TheCommand.Data(1..TheCommand.Length) :=
            "tokenReader.getState('" & Reader & "')";

         TcpIp.SendAndReceive ( IsAdmin  => False,
                                Outgoing => TheCommand,
                                Incoming => TheReturn,
                                Success  => Success);

         if Success then
            declare
               MainDict : MsgProc.DictionaryT := MsgProc.GetDictionary(
                  Msg => TheReturn.Data, Arg => 1);

               Card_Handle : String :=
                  MsgProc.GetStringByKey(Dic => MainDict, Key => "cardHandle");

               Reader_Name : String :=
                  MsgProc.GetStringByKey(Dic => MainDict, Key => "name");
            begin

               ReaderState.Card_State := TokenAPI.CardStateT'Value(
                     MsgProc.GetStringByKey(
                        Dic => MainDict,
                        Key => "cState"));

               ReaderState.Card_Length := Card_Handle'Length;
               ReaderState.Card_Handle(1..ReaderState.Card_Length) := Card_Handle;

               ReaderState.Reader_State := TokenAPI.ReaderStateT'Value(
                     MsgProc.GetStringByKey(
                        Dic => MainDict,
                        Key => "rState"));

               ReaderState.Reader_Length := Reader_Name'Length;
               ReaderState.Reader_Name(1..ReaderState.Reader_Length) := Reader_Name;

            end;
         else
            -- Arbitrary default
            ReaderState := TokenReaderStateT'(
               Card_State => TokenAPI.Absent,
               Card_Handle  => (others => ASCII.Nul),
               Card_Length => 1,
               Reader_Length => 1,
               Reader_State => TokenAPI.Unaware,
               Reader_Name  => (others => ASCII.Nul));
         end if;

         return ReaderState;
      end GetReaderState;

      procedure UpdateState
      is
      begin
         State := TisStateT'(
            Door      => GetDoorState,
            Display   => GetDisplayState,
            Bio       => GetBioState,
            Alarm     => GetAlarmState,
            -- Note: Card reader names MUST be 8 characters!
            ExtReader => GetReaderState("EXTREAD "),
            IntReader => GetReaderState("INTREAD "));
      end UpdateState;

      -- Door open routine:
      procedure Open_Door
      is
         TheCommand : TcpIp.MessageT;
         TheReturn  : TcpIp.MessageT;
         Success    : Boolean;
      begin
         TheCommand.Length := 11;
         TheCommand.Data(1..TheCommand.Length) := "door.open()";
         TcpIp.SendAndReceive ( IsAdmin  => False,
                                Outgoing => TheCommand,
                                Incoming => TheReturn,
                                Success  => Success);
      end Open_Door;

      procedure Close_Door
      is
         TheCommand : TcpIp.MessageT;
         TheReturn  : TcpIp.MessageT;
         Success    : Boolean;
      begin
         TheCommand.Length := 12;
         TheCommand.Data(1..TheCommand.Length) := "door.close()";
         TcpIp.SendAndReceive ( IsAdmin  => False,
                                Outgoing => TheCommand,
                                Incoming => TheReturn,
                                Success  => Success);
      end Close_Door;

      procedure Send_Fingerprint( Image : in String;
                                  FAR_val, FRR_Val : in BioAPI.RateT)
      is
         TheCommand : TcpIp.MessageT;
         TheReturn  : TcpIp.MessageT;
         Success    : Boolean;

         ImageTrim : String := Ada.Strings.Fixed.Trim(Image, Ada.Strings.Both);
         FAR : String := BioAPI.RateT'Image(FAR_Val);
         FRR : String := BioAPI.RateT'Image(FRR_Val);
      begin

         TheCommand.Length := 66 + ImageTrim'Length + FAR'Length + FRR'Length;
         TheCommand.Data(1..TheCommand.Length) :=
            "bioDevice.supplyImage({'imageTemplate': '" & ImageTrim & "', 'FAR': '" &
            FAR & "', 'FRR': '" & FRR & "'})";

         TcpIp.SendAndReceive ( IsAdmin  => False,
                                Outgoing => TheCommand,
                                Incoming => TheReturn,
                                Success  => Success);
      end Send_Fingerprint;

      procedure Insert_Token(Reader: in String;
                             CardID: in CommonTypes.Unsigned32T)
      is
         TheCommand : TcpIp.MessageT;
         TheReturn  : TcpIp.MessageT;
         Success    : Boolean;
         CardStr    : String := CommonTypes.Unsigned32T'Image(CardID);
      begin
         TheCommand.Length := 29 + Reader'Length + CardStr'Length;
         TheCommand.Data(1..TheCommand.Length) :=
            "tokenReader.insertCard('" & Reader & "','" & CardStr & "')";

         TcpIp.SendAndReceive ( IsAdmin  => False,
                                Outgoing => TheCommand,
                                Incoming => TheReturn,
                                Success  => Success);
      end Insert_Token;


      procedure Remove_Token(Reader: in String)
      is
         TheCommand : TcpIp.MessageT;
         TheReturn  : TcpIp.MessageT;
         Success    : Boolean;

      begin
         TheCommand.Length := 26 + Reader'Length;
         TheCommand.Data(1..TheCommand.Length) :=
            "tokenReader.removeCard('" & Reader & "')";

         TcpIp.SendAndReceive ( IsAdmin  => False,
                                Outgoing => TheCommand,
                                Incoming => TheReturn,
                                Success  => Success);
      end Remove_Token;

      -- Send a command to the simulator:
      procedure SendCommand( TheCommand : in  TcpIP.MessageT;
                             Success    : out Boolean)
      is
         DontCare : TcpIp.MessageT;
      begin
         TcpIp.SendAndReceive ( IsAdmin  => False,
                                Outgoing => TheCommand,
                                Incoming => DontCare,
                                Success  => Success);
      end SendCommand;

   end TisState;

   -- Create_Card_Message
   -- This code has been lifted pretty much bodily from makecard.adb :-)
   --
   -- Takes the name of a <card>.dat file, reads the contents and constructs
   -- an appropriate message to send to the SPRE simulator for storing the
   -- card details.
   --
   -- As it is reading the file, it watches for the "TemplateID" line and
   -- returns this in the "FingerPrint" parameter.
   --
   -- The message is passed back in the CardCommand parameter and needs to be
   -- sent to the simulator by calling TisState.SendCommand.
   --
   -- This subprogram is outside the protected object TisState in order to
   -- minimise the amount of time in the object.
   procedure Create_Card_Message ( CardDir     : in  String;
                                   CardName    : in  String;
                                   CardHandle  : in  CommonTypes.Unsigned32T;
                                   FingerPrint : out BioAPI.IDT;
                                   CardCommand : out TcpIp.MessageT;
                                   Success     : out Boolean)
   is
      TheCommand : TcpIp.MessageT;
      InFile     : Ada.Streams.Stream_IO.File_Type;
      TheChar    : Character;
      LocalMsg   : String(TcpIP.MessageIndexT);
      ValStart, Key1, Key2, Key3, Key4
                 : Integer;
      LocalHandle : String := CommonTypes.Unsigned32T'Image(CardHandle);
   begin
      Ada.Streams.Stream_IO.Open
        (InFile, Ada.Streams.Stream_IO.In_File, CardDir & CardName);

      declare
         CommandText : String := "cardDB.putCard( '" & LocalHandle & "', '";
      begin
         TheCommand.Length := CommandText'Length;
         TheCommand.Data(1 .. TheCommand.Length) := CommandText;
      end;

      while not Ada.Streams.Stream_IO.End_Of_File(InFile) loop
         Character'Read(Ada.Streams.Stream_IO.Stream(InFile), TheChar);
         --if TheChar = ''' then
            -- escape this
            --TheCommand.Length := TheCommand.Length + 1;
            --TheCommand.Data(TheCommand.Length) := '\';
         --end if;
         if TheChar /= Ascii.CR and TheChar /= Ascii.LF then
            TheCommand.Length := TheCommand.Length + 1;
            TheCommand.Data(TheCommand.Length) := TheChar;
         end if;
      end loop;
      TheCommand.Length := TheCommand.Length + 1;
      TheCommand.Data(TheCommand.Length) := ''';
      TheCommand.Length := TheCommand.Length + 1;
      TheCommand.Data(TheCommand.Length) := ')';

      Ada.Streams.Stream_IO.Close(InFile);

      -- Find the fingerprint template:
      LocalMsg(1..TheCommand.Length) := TheCommand.Data(1..TheCommand.Length);
      ValStart := Ada.Strings.Fixed.Index(Source  => LocalMsg,
                                          Pattern => "'TemplateID'");

      -- Remove everything upto and including the TemplateID key:
      Ada.Strings.Fixed.Delete(Source  => LocalMsg,
                               From    => 1,
                               Through => ValStart + 11);

      -- Trim up to the next '
      ValStart := Ada.Strings.Fixed.Index(Source  => LocalMsg,
                                          Pattern => "'");
      Ada.Strings.Fixed.Delete(Source  => LocalMsg,
                               From    => 1,
                               Through => ValStart );

      -- Find the closing ' for the template:
      ValStart := Ada.Strings.Fixed.Index(Source  => LocalMsg,
                                          Pattern => "'");

      ValStart := ValStart -1;

      if ValStart > Fingerprint'Last then
         ValStart := Fingerprint'Last;
      end if;

      FingerPrint := (others => ' ');
      FingerPrint(1..ValStart) := LocalMsg(1..ValStart);

      --Key1 := Ada.Strings.Fixed.Index(Source => TheCommand.Data, Pattern => "***END");
      --if Key1 /= 0 then
         --TheCommand.Data(1 .. TheCommand.Length - 6) :=
            --TheCommand.Data(1 .. Key1-1) &  TheCommand.Data(Key1 + 6 .. TheCommand.Length);
         --TheCommand.Length := TheCommand.Length - 6;
      --end if;

      -- Check to see that we have just loaded a card definition.
      -- We do this by searching for three distinct keys that should be
      -- present in all cards. We could probably get away with just searching
      -- for one of these...
      Key1 := Ada.Strings.Fixed.Index(Source => TheCommand.Data, Pattern => "RawIACert");
      --if Key1 /= 0 then
         --TheCommand.Data(1 .. TheCommand.Length + 1) :=
            --TheCommand.Data(1 .. Key1-1) & ",'IACERT':" & TheCommand.Data(Key1 + 9 .. TheCommand.Length);
         --TheCommand.Length := TheCommand.Length + 1;
      --end if;

      Key2 := Ada.Strings.Fixed.Index(Source => TheCommand.Data, Pattern => "RawIDCert");
      --if Key2 /= 0 then
         --TheCommand.Data(1 .. TheCommand.Length + 1) :=
            --TheCommand.Data(1 .. Key2-1) & ",'IDCERT':" & TheCommand.Data(Key2 + 9 .. TheCommand.Length);
         --TheCommand.Length := TheCommand.Length + 1;
      --end if;

      Key3 := Ada.Strings.Fixed.Index(Source => TheCommand.Data, Pattern => "RawAuthCert");
      --if Key3 /= 0 then
         --TheCommand.Data(1 .. TheCommand.Length + 1) :=
            --TheCommand.Data(1 .. Key3-1) & ",'AUTHCERT':" & TheCommand.Data(Key3 + 11 .. TheCommand.Length);
         --TheCommand.Length := TheCommand.Length + 1;
      --end if;

      Key4 := Ada.Strings.Fixed.Index(Source => TheCommand.Data, Pattern => "RawPrivCert");
      --if Key4 /= 0 then
         --TheCommand.Data(1 .. TheCommand.Length + 1) :=
            --TheCommand.Data(1 .. Key4-1) & ",'PRIVCERT':" & TheCommand.Data(Key4 + 11 .. TheCommand.Length);
         --TheCommand.Length := TheCommand.Length + 1;
      --end if;

      -- If any of our KeyN values ar 0, we haven't loaded a card:
      if Key1 = 0 or Key2 = 0 or Key3 = 0 or Key4 = 0 then
         CardCommand := TcpIp.NullMsg;
         Success := False;
      else
         CardCommand := TheCommand;
         Success := True;
      end if;

   end Create_Card_Message;

   ------------------------------------------------------------------
   -- InitTestDevices
   --
   -- Description:
   --    Performs the Initialisation of the
   --    TCP/IP interface.
   --
   -- Traceunit: C.TIS.InitTestDevices
   ------------------------------------------------------------------
   procedure InitTestDevices(OK : out Boolean)
   is
      procedure InitDevices (Success : out Boolean)
      is

         DoorInit   : constant String := "door.setState({'closed?': 'TRUE', 'locked?': 'TRUE', 'operational?': 'TRUE'})";
         BioInit    : constant String := "bioDevice.setState({'operational?': 'TRUE', 'noImage?': 'TRUE', 'image':{'imageTemplate':'','FAR':'0', 'FRR':'0'} })";
         Token1Init : constant String := "tokenReader.setState('EXTREAD ', {'cState': 'absent','cardHandle':'','rState': 'empty', 'name': 'EXTREAD '})";
         Token2Init : constant String := "tokenReader.setState('INTREAD ', {'cState': 'absent','cardHandle':'','rState': 'empty', 'name': 'INTREAD '})";

         StageSuccess : Boolean;

         procedure SendInitString (InitString : in String; InitSuccess : out Boolean)
         is
            TheCommand  : TcpIp.MessageT;
            JunkReturn  : TcpIp.MessageT;
         begin
            TheCommand.Length := InitString'Length;
            TheCommand.Data(1..TheCommand.Length) := InitString;
            TcpIp.SendAndReceive ( IsAdmin  => False,
                                   Outgoing => TheCommand,
                                   Incoming => JunkReturn,
                                   Success  => InitSuccess);
         end SendInitString;

      begin

         SendInitString (InitString => DoorInit,   InitSuccess => StageSuccess);
         Success := StageSuccess;
         SendInitString (InitString => BioInit,    InitSuccess => StageSuccess);
         Success := Success and StageSuccess;

         -- **************************************************************
         -- *** Commented out until token reader is implemented in SIM ***
         -- **************************************************************

         SendInitString (InitString => Token1Init, InitSuccess => StageSuccess);
         Success := Success and StageSuccess;
         SendInitString (InitString => Token2Init, InitSuccess => StageSuccess);
         Success := Success and StageSuccess;

      end InitDevices;

   begin
      TCPIP.Init(OK);
      if OK then
         TCPIP.OpenAll(OK);
         if not OK then
            Ada.Text_IO.Put_Line("Failed to connect to SPRE");
         else
            InitDevices(OK);
            if not OK then
               Ada.Text_IO.Put_Line("Failed to initialise SPRE state");
            end if;
         end if;
      end if;
   end InitTestDevices;

   ------------------------------------------------------------------
   -- FinaliseTestDevices
   --
   -- Description:
   --    Performs the Finalisation of the
   --    TCP/IP interface.
   --
   -- Traceunit: C.TIS.FinaliseTestDevices
   ------------------------------------------------------------------
   procedure FinaliseTestDevices
   --# derives ;
   is
   --# hide FinaliseTestDevices;
   begin
      TCPIP.CloseAll;
   end FinaliseTestDevices;

end SPRE_Interface;
