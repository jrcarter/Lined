with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Lined.Buffer;
with Lined.Line_Numbers;
with Lined.Searching;

procedure Lined.Program is
   procedure Check_Global
      (Command : in String; Last : out Natural; Global : out Boolean; Start : out Natural; Stop : out Natural);
   -- if Command starts with 'g' or 'x', marks lines for global processing
   -- Global indicates if a global prefix was encountered

   procedure Process (Command : in String; Global : in Boolean; Current : in out Natural);
   -- Handle a single command after initial line-number parsing
   -- Global indicates if Command is being processed under a global prefix

   procedure Process_Global
      (Command : in String; Start : in Natural; Current : in out Natural; Curr_Save : in out Natural; Success : out Boolean)
   with
      Pre => Start <= Buffer.Last;
   -- Passes Command to Process for every line in the buffer that is marked
   -- Current is the current line number, which may be changed by Command
   -- Curr_Save becomes the current line number at the time of processing the last line processed, for resetting after a failure
   -- (in out so we don't overwrite the actual with garbage in the unlikely case that no lines are marked)
   -- Success is True if everything went well; False if anything raised an exception; we use this flag to ensure that Current will
   -- copy back the correct value in case of failure

   procedure Set_Lines (Default_Start : in Natural; Default_Stop : in Natural; Start : in out Natural; Stop : in out Natural)
   with
      Pre  => Default_Start <= Buffer.Last and Default_Stop <= Buffer.Last and Default_Start <= Default_Stop,
      Post => (if Line_Numbers.Num_Numbers = 0 then Start = Default_Start and Stop = Default_Stop) and
                Start <= Stop and Stop <= Buffer.Last;
   -- Sets Start and Stop to the defaults used if values have not been specified
   -- Raises Invalid_Input if the values are not suitable for the print command

   Global_Indicator : constant Character := 'g';

   procedure Check_Global (Command : in String; Last : out Natural; Global : out Boolean; Start : out Natural; Stop : out Natural)
   is
      Index   : Natural := Ada.Strings.Fixed.Index_Non_Blank (Command);
      Include : Boolean;
   begin -- Check_Global
      Start  := Line_Numbers.Start;
      Stop   := Line_Numbers.Stop;
      Last   := Command'First - 1;
      Global := False;

      if Index = 0 or else (Command (Index) /= Global_Indicator and Command (Index) /= 'x') then
         return;
      end if;

      Global := True;
      Include := Command (Index) = Global_Indicator;
      Index := Index + 1;

      Last := Searching.Terminator (Command (Index + 1 .. Command'Last), Command (Index) );

      if Last > Index + 1 then
         Searching.Process (Command (Index + 1 .. Last - 1) );
      end if;

      Set_Lines (Default_Start => 1, Default_Stop => Buffer.Last, Start => Start, Stop => Stop);
      Buffer.Clear_Marks;

      Mark_Lines : for I in Start .. Stop loop
         Buffer.Mark (Number => I, Status => Searching.Search (Buffer.Line (I) ).Found = Include);
      end loop Mark_Lines;
   end Check_Global;

   procedure Process (Command : in String; Global : in Boolean; Current : in out Natural) is separate;

   procedure Process_Global
      (Command : in String; Start : in Natural; Current : in out Natural; Curr_Save : in out Natural; Success : out Boolean)
   is
      Line  : Natural := Start;
      Count : Natural := 0;
      Last  : Natural;
   begin -- Process_Global
      Success := True;

      -- Some commands can scramble the buffer pretty well, so we keep looking for marked lines and processing them until there
      -- are none left

      Match_All : loop
         if not Buffer.Marked (Line) then
            Line := Buffer.Next (Line);
            Count := Count + 1;
         else
            Buffer.Mark (Number => Line, Status => False);
            Current := Line;
            Curr_Save := Line;
            Line_Numbers.Parse (Command => Command, Current => Current, Last => Last);
            Process (Command => Command (Last + 1 .. Command'Last), Global => True, Current => Current);
            Count := 0;

            if Line > Buffer.Last then
               Line := Buffer.Last;
            end if;
         end if;

         exit Match_All when Count > Buffer.Last;
      end loop Match_All;
   exception -- Process_Global
   when others =>
      Success := False;
   end Process_Global;

   procedure Set_Lines (Default_Start : in Natural; Default_Stop : in Natural; Start : in out Natural; Stop : in out Natural) is
      -- Empty
   begin -- Set_Lines
      if Line_Numbers.Num_Numbers = 0 then
         Start := Default_Start;
         Stop  := Default_Stop;
      end if;

      if Start > Stop or Start < 1 then
         raise Invalid_Input;
      end if;
   end Set_Lines;

   Global  : Boolean;
   Start   : Natural;
   Stop    : Natural;
   Current : Natural := 0;
begin -- Lined.Program
   if Ada.Command_Line.Argument_Count > 0 then
      Buffer.Load (File_Name => Ada.Command_Line.Argument (1) );
      Ada.Text_IO.Put_Line (Item => Buffer.Last'Image);
   end if;

   All_Commands : loop
      One_Line : declare
         Line : constant String := Ada.Text_IO.Get_Line;

         Curr_Save : Natural := Current;
         Last      : Natural;
         Success   : Boolean;
      begin -- One_Line
         exit All_Commands when Line = "q";

         Line_Numbers.Parse (Command => Line, Current => Current, Last => Last);
         Check_Global (Command => Line (Last + 1 .. Line'Last), Last => Last, Global => Global, Start => Start, Stop => Stop);

         if not Global then
            Process (Command => Line (Last + 1 .. Line'Last), Global => False, Current => Current);
         else
            Process_Global (Command => Line (Last + 1 .. Line'Last),
                            Start   => Start,
                            Current => Current,
                            Curr_Save => Curr_Save,
                            Success => Success);

            if not Success then
               raise Invalid_Input;
            end if;
         end if;
      exception -- One_Line
      when others =>
         Ada.Text_IO.Put_Line (Item => "?");
         Current := Integer'Min (Curr_Save, Buffer.Last);
      end One_Line;
   end loop All_Commands;
exception -- Lined.Program
when Ada.Text_IO.End_Error =>
   null; -- End_Of_File on terminal input requires look ahead, so this way of handling EOF gives proper interactive behavior
end Lined.Program;
