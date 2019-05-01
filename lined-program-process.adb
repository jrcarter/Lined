with Ada.Containers.Indefinite_Vectors;

separate (Lined.Program)
procedure Process (Command : in String; Global : in Boolean; Current : in out Natural) is
   procedure Append (After : in Natural) with
      Pre => After <= Buffer.Last;
   -- Reads lines from standard input and adds them after line After
   -- Lines are terminated by a line containing only '.'

   procedure Change (Start : in Positive; Stop : in Positive) with
      Pre  => Start <= Stop and Stop <= Buffer.Last;
   -- Replaces the existing lines Start .. Stop with input as in Append or Insert
   -- The current line will be set to the last input line added

   procedure Delete (Start : in Positive; Stop : in Positive; Current : in out Natural) with
      Pre  => Start <= Stop and Stop <= Buffer.Last;
   -- Deletes lines Start .. Stop
   -- If Stop was the old Buffer.Last, Current will be the new Buffer.Last
   -- Otherwise, Current will be Start (which now is the line after the last line deleted)

   procedure Edit (Command : in String) with
      Post => (if File_Name (Command) /= "" then Buffer.Default_File = File_Name (Command) );
   -- If File_Name (Command) /= "", calls Buffer.Load with File_Name (Command) and prints Buffer.Last
   -- Otherwise raises Invalid_Input

   function File_Name (Command : String) return String;
   -- If Trim (Command) = "", returns Buffer.Default_File
   -- Otherwise, returns Trim (Command)

   procedure Insert (Before : in Positive; Current : in out Natural) with
      Pre => Before <= Buffer.Last + 1;
   -- Reads lines from standard input and adds them before line Before
   -- Current will be set to the last line added

   procedure Move (Start : in Positive; Stop : in Positive; Destination : in Natural; Current : in out Natural) with
      Pre  => Start <= Stop and Stop <= Buffer.Last and Destination <= Buffer.Last and Destination not in Start .. Stop - 1;
   -- Moves lines Start .. Stop after Destination
   -- The current line will be set to the last line moved

   procedure Print (Start : in Positive; Stop : in Positive; Current : in out Natural) with
      Pre  => Start <= Stop and Stop <= Buffer.Last,
      Post => Stop = Current;
   -- Prints the lines Start .. Stop

   Print_Indicator : constant Character := 'p';

   function Printing_Requested (Command : String) return Boolean with
      Post => Printing_Requested'Result = (Command'Length > 0                              and then
                                           Ada.Strings.Fixed.Index_Non_Blank (Command) > 0 and then
                                           Command (Ada.Strings.Fixed.Index_Non_Blank (Command) ) = Print_Indicator);

   procedure Read (Command : in String; Current : in out Natural) with
      Post => (if File_Name (Command) /= "" then Buffer.Default_File = File_Name (Command) );
   -- If File_Name (Command) /= "", reads lines from File_Name (Command), appends them after Line_Numbers.Stop, and prints the
   -- number of lines read
   -- Otherwise raises Invalid_Input

   procedure Show_File (Command : in String);
   -- If Trim (Command) /= "", passes the result to Buffer.Set_File
   -- In all cases, then prints Buffer.Default_File

   procedure Substitute (Command : in String; Current : in out Natural; Last : out Natural; Printing : out Boolean);
   -- Parses out a regular expression and replacement string from Command, along with the multiple-replacement flag 'g' and the
   -- Printing flag 'p' at the end
   -- /pattern/replacement/gp
   -- Replaces the first or all occurrence of the pattern with the replacement string, depending on whether the
   -- multiple-replacement flag is present in the specified lines
   -- Occurrences of '@' in the replacement string are replaced with the matched text
   -- If any parsing fails, or no replacements were made, raises Invalid_Input

   procedure Write (Name : in String; Start : in Positive; Stop : in Positive) with
      Pre  => Name /= "",
      Post => Buffer.Default_File = Name;
   -- Creates Name, writes Start .. Stop to Name, closes Name, and calls Buffer.Set_File with Name

   procedure Append (After : in Natural) is
      -- Empty
   begin -- Append
      if Global then
         raise Invalid_Input;
      end if;

      Insert (Before => After + 1, Current => Current);
   end Append;

   procedure Change (Start : in Positive; Stop : in Positive) is
      Deleted_Last : constant Boolean := Stop = Buffer.Last;
   begin -- Change
      if Global then
         raise Invalid_Input;
      end if;

      Delete (Start => Start, Stop => Stop, Current => Current);
      Insert (Before => (if Deleted_Last then Buffer.Last + 1 else Start), Current => Current);
   end Change;

   procedure Delete (Start : in Positive; Stop : in Positive; Current : in out Natural) is
      Deleted_Last : constant Boolean := Stop = Buffer.Last;
   begin -- Delete
      Delete_All : for I in Start .. Stop loop
         Buffer.Delete (Number => Start);
      end loop Delete_All;

      Current := (if Deleted_Last then Buffer.Last else Start);
   end Delete;

   procedure Edit (Command : in String) is
      Name : constant String := File_Name (Command);
   begin -- Edit
      if Name = "" or Global then
         raise Invalid_Input;
      end if;

      Buffer.Load (File_Name => Name);
      Ada.Text_IO.Put_Line (Item => Buffer.Last'Image);
   end Edit;

   function File_Name (Command : String) return String is
      Name : constant String := Ada.Strings.Fixed.Trim (Command, Ada.Strings.Both);
   begin -- File_Name
      if Global then
         raise Invalid_Input;
      end if;

      if Name = "" then
         return Buffer.Default_File;
      end if;

      return Name;
   end File_Name;

   procedure Insert (Before : in Positive; Current : in out Natural) is
      Line : Positive := Before;
   begin -- Insert
      if Global then
         raise Invalid_Input;
      end if;

      Current := Before - 1;

      Get_Lines : loop
         One_Line : declare
            Text : constant String := Ada.Text_IO.Get_Line;
         begin -- One_Line
            exit Get_Lines when Text = ".";

            Buffer.Insert (Line => Text, Before => Line);
            Current := Line;
            Line := Line + 1;
         end One_Line;
      end loop Get_Lines;
   end Insert;

   procedure Move (Start : in Positive; Stop : in Positive; Destination : in Natural; Current : in out Natural) is
      package Line_Lists is new Ada.Containers.Indefinite_Vectors (Index_Type => Positive, Element_Type => String);

      Before : Positive := Destination + 1;
      Copy   : Line_Lists.Vector; -- Copy of lines Start .. Stop if needed
   begin -- Move
      if Before = Start or Before = Stop + 1 then -- Lines are already at their destination
         return;
      end if;

      if Before < Start then -- Copy and delete before inserting
         Copy_All : for I in Start .. Stop loop
            Copy.Append (New_Item => Buffer.Line (I) );
         end loop Copy_All;

         Delete (Start => Start, Stop => Stop, Current => Current);

         Insert_Copy : for S of Copy loop
            Buffer.Insert (Line => S, Before => Before);
            Current := Before;
            Before := Before + 1;
         end loop Insert_Copy;

         return;
      end if;
      -- Before > Stop; no copy needed

      Move_All : for I in Start .. Stop loop
         Buffer.Insert (Line => Buffer.Line (I), Before => Before);
         Before := Before + 1;
      end loop Move_All;

      Delete (Start => Start, Stop => Stop, Current => Current);
      Current := Before - 1 - (Stop - Start + 1);
   end Move;

   procedure Print (Start : in Positive; Stop : in Positive; Current : in out Natural) is
      -- Empty
   begin -- Print
      All_Lines : for I in Start .. Stop loop
         Ada.Text_IO.Put_Line (Item => Buffer.Line (I) );
      end loop All_Lines;

      Current := Stop;
   end Print;

   function Printing_Requested (Command : String) return Boolean is
      Index : constant Natural := Ada.Strings.Fixed.Index_Non_Blank (Command);
   begin -- Printing_Requested
      if Index in Command'Range and then Index < Command'Last then
         raise Invalid_Input;
      end if;

      if Index not in Command'Range then
         return False;
      end if;

      return Command (Index) = Print_Indicator;
   end Printing_Requested;

   procedure Read (Command : in String; Current : in out Natural) is
      Name : constant String := File_Name (Command);

      File  : Ada.Text_IO.File_Type;
      Count : Natural := 0;
   begin -- Read
      if Name = "" then
         raise Invalid_Input;
      end if;

      Ada.Text_IO.Open (File => File, Mode => Ada.Text_IO.In_File, Name => Name);
      Buffer.Set_File (Name => Name);

      Handle_EOF : declare
         Line : Positive := Line_Numbers.Stop + 1;
      begin -- Handle_EOF
         Current := Line - 1;

         All_Lines : loop
            Buffer.Insert (Line => Ada.Text_IO.Get_Line (File), Before => Line);
            Current := Line;
            Line := Line + 1;
            Count := Count + 1;
         end loop All_Lines;
      exception -- Handle_EOF
         when Ada.Text_IO.End_Error =>
            null; -- Safe way to handle EOF
      end Handle_EOF;

      Ada.Text_IO.Close (File => File);
      Ada.Text_IO.Put_Line (Item => Count'Image);
   end Read;

   procedure Show_File (Command : in String) is
      Name : constant String := File_Name (Command);
   begin -- Show_File
      if Global then
         raise Invalid_Input;
      end if;

      if Name = "" then
         Ada.Text_IO.Put_Line (Item => Buffer.Default_File);
      else
         Buffer.Set_File (Name => Name);
         Ada.Text_IO.Put_Line (Item => Name);
      end if;
   end Show_File;

   procedure Substitute (Command : in String; Current : in out Natural; Last : out Natural; Printing : out Boolean) is separate;

   procedure Write (Name : in String; Start : in Positive; Stop : in Positive) is
      File : Ada.Text_IO.File_Type;
   begin -- Write
      if Start = 1 and Stop = Buffer.Last then
         Buffer.Write (File_Name => Name);
         Ada.Text_IO.Put_Line (Item => Buffer.Last'Image);

         return;
      end if;

      Ada.Text_IO.Create (File => File, Name => Name);
      Buffer.Set_File (Name => Name);

      All_Lines : for I in Start .. Stop loop
         Ada.Text_IO.Put_Line (File => File, Item => Buffer.Line (I) );
      end loop All_Lines;

      Ada.Text_IO.Close (File => File);
      Ada.Text_IO.Put_Line (Item => Integer'Max (Stop - Start + 1, 0)'Image);
   end Write;

   Start    : Natural := Line_Numbers.Start;
   Stop     : Natural := Line_Numbers.Stop;
   Printing : Boolean := False;
   Last     : Natural;
begin -- Process
   if Command = "" then -- Print next or specified line
      if Line_Numbers.Num_Numbers = 0 then
         Stop := Buffer.Next (Current);
         Print (Start => Stop, Stop => Stop, Current => Current);

         return;
      end if;

      Print (Start => Stop, Stop => Stop, Current => Current);

      return;
   end if;

   case Command (Command'First) is
   when '=' => -- Print specified or current line number
      Printing := Printing_Requested (Command (Command'First + 1 .. Command'Last) );
      Ada.Text_IO.Put_Line (Item => Line_Numbers.Stop'Image);
      Current := Line_Numbers.Stop;
   when 'a' => -- Append after specified or current line
      if Command'Length > 1 then
         raise Invalid_Input;
      end if;

      Append (After => Line_Numbers.Stop);
   when 'c' => -- Change specified or current line(s)
      if Command'Length > 1 then
         raise Invalid_Input;
      end if;

      Set_Lines (Default_Start => Current, Default_Stop => Current, Start => Start, Stop => Stop);
      Change (Start => Start, Stop => Stop);
   when 'd' => -- Delete specified or current line(s)
      Printing := Printing_Requested (Command (Command'First + 1 .. Command'Last) );
      Set_Lines (Default_Start => Current, Default_Stop => Current, Start => Start, Stop => Stop);
      Delete (Start => Start, Stop => Stop, Current => Current);
   when 'e' => -- Edit specified or default file
      if Line_Numbers.Num_Numbers /= 0 then
         raise Invalid_Input;
      end if;

      Edit (Command (Command'First + 1 .. Command'Last) );
   when 'f' => -- show or change default File name
      if Line_Numbers.Num_Numbers /= 0 then
         raise Invalid_Input;
      end if;

      Show_File (Command (Command'First + 1 .. Command'Last) );
   when 'i' => -- Insert before specified or current line number
      if Command'Length > 1 then
         raise Invalid_Input;
      end if;

      Insert (Before => Line_Numbers.Stop, Current => Current);
   when 'm' => -- (.,.)m Line: Move specified or current line(s) to after Line
      Get_Destination : declare
         Index       : Natural;
         Destination : Natural;
      begin -- Get_Destination
         Line_Numbers.Get_Line_Number
            (Source => Command (Command'First + 1 .. Command'Last), Current => Current, Last => Index, Value => Destination);

         if Index < Command'First + 1 then -- No destination
            raise Invalid_Input;
         end if;

         Printing := Printing_Requested (Command (Index + 1 .. Command'Last) );
         Set_Lines (Default_Start => Current, Default_Stop => Current, Start => Start, Stop => Stop);
         Move (Start => Start, Stop => Stop, Destination => Destination, Current => Current);
      end Get_Destination;
   when Print_Indicator => -- Print specified or current line(s)
      if Command'Length > 1 then
         raise Invalid_Input;
      end if;

      Set_Lines (Default_Start => Current, Default_Stop => Current, Start => Start, Stop => Stop);
      Print (Start => Start, Stop => Stop, Current => Current);
   when 'r' => -- Read lines from specified or default file and append them after specified or current line
      Read (Command (Command'First + 1 .. Command'Last), Current => Current);
   when 's' => -- Replace specified or default pattern by specified replacement pattern
      Substitute (Command => Command (Command'First + 1 .. Command'Last), Last => Last, Printing => Printing, Current => Current);
   when 'w' => -- Write specified lines or the entire buffer to specified or default file
      Set_Lines (Default_Start => 1, Default_Stop => Buffer.Last, Start => Start, Stop => Stop);
      Write (Name => File_Name (Command (Command'First + 1 .. Command'Last) ), Start => Start, Stop => Stop);
   when others =>
      raise Invalid_Input;
   end case;

   Stop := Current;

   if Printing and Stop in 1 .. Buffer.Last then
      Print (Start => Stop, Stop => Stop, Current => Current);
   end if;
end Process;
