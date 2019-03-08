------------------------------------------------------------------
-- Copyright 2004 Praxis Critical Systems Ltd
-- Graphical User Interface demo
--
-- File:
--    $Id: spre_interface.ads,v 1.4 2004/04/09 21:28:50 mjw Exp $
--
------------------------------------------------------------------
with Ada.Strings.Fixed;
with Ada.Text_IO;
with TcpIP;
with DoorApi;
with BioAPI;
with TokenAPI;
with MsgProc;
with CommonTypes;

use type TcpIP.MessageT;
use type BioAPI.RateT;

package SPRE_Interface is 

   -- Door state
   type DoorStateT is record
      Closed : Boolean;
      Locked : Boolean;
   end record;
   
   -- Display state
   MaxStringLength : constant := 1000;
   subtype IndexT is Natural range 0 .. MaxStringLength;
   subtype StringT is String (1 .. IndexT'Last);

   type DisplayStateT is record
      TopString          : StringT;
      TopStringLength    : IndexT;
      TopScroll          : Boolean;
      TopDisplayWidth    : IndexT;
      BottomString       : StringT;
      BottomStringLength : IndexT;
      BottomDisplayWidth : IndexT;
   end record;
   
   -- Bio (fingerprint reader) state
   type BioStateT is record
      NoImage       : Boolean;
      ImageTemplate : BioAPI.IDT;
      ImageLength   : Natural;
      FAR           : BioAPI.RateT;
      FRR           : BioAPI.RateT;
   end record;
   
   -- Alarm state
   type AlarmStateT is record
      Active : Boolean;
   end record;
   
   -- Token (card) reader state
   type TokenReaderStateT is record
      Card_State     : TokenAPI.CardStateT;
      Card_Length    : Natural;
      Card_Handle    : StringT;
      Reader_State   : TokenAPI.ReaderStateT;
      Reader_Length  : Natural;
      Reader_Name    : StringT;
   end record;

   -- Define a type to hold the entire TIS interface state
   type TisStateT is record
      Door      : DoorStateT;
      Display   : DisplayStateT;
      Bio       : BioStateT;
      Alarm     : AlarmStateT;
      ExtReader : TokenReaderStateT;
      IntReader : TokenReaderStateT;
   end record;
   
   -- Procedures for the initialisation and finalisation of the test devices:
   procedure InitTestDevices (OK : out Boolean);

   procedure FinaliseTestDevices;

   procedure Create_Card_Message ( CardDir     : in  String;
                                   CardName    : in  String;
                                   CardHandle  : in  CommonTypes.Unsigned32T;
                                   FingerPrint : out BioAPI.IDT;
                                   CardCommand : out TcpIp.MessageT;
                                   Success     : out Boolean);

   -- Hold the TIS state within a protected object, this makes access by the
   -- GUI thread safe. Also, all operations that access the socket interface 
   -- should be placed in here to make thread safe.
   protected TisState is
   
      -- Get updated state from SPRE/SIM, declared inside the protected object
      -- to ensure thread safe access to the socket.
      procedure UpdateState;
      
      function CurrentState return TisStateT;

      -- Door Open and close routines:
      procedure Open_Door;
      procedure Close_Door;

      -- BioDevice
      procedure Send_Fingerprint( Image : in String;
                                  FAR_val, FRR_Val : in BioAPI.RateT);

      -- TokenReader
      procedure Insert_Token(Reader: in String; CardID: in CommonTypes.Unsigned32T);

      procedure Remove_Token(Reader: in String);

      procedure SendCommand( TheCommand : in  TcpIP.MessageT;
                             Success    : out Boolean);
   private

      State : TisStateT;
      
   end TisState;

end SPRE_Interface;   
