-- The buffer of lines being edited
-- Lines are numbered from 1 (the first line) to Last
-- Line numbers are relative: when lines are added or deleted, the numbering of the other lines may change

package Lined.Buffer with SPARK_Mode, Abstract_State => State, Initializes => State is
   procedure Clear
   with
      Global => (Output => State),
      Post   => Last = 0;
   -- Makes the buffer empty
   -- The buffer is initially empty

   procedure Mark (Number : in Positive; Status : in Boolean)
   with
      Global => (In_Out => State),
      Pre    => Number in 1 .. Last,
      Post   => Marked (Number) = Status;
   -- Marks (Status True) or unmarks (Status False) line Number

   function Marked (Number : Positive) return Boolean With Global => (Input => State);
   -- Returns the mark status of line Number

   procedure Clear_Marks With Global => (In_Out => State),
      Post   => (for all I in 1 .. Last => not Marked (I) );
   -- Sets the marks for all lines in the buffer to False

   procedure Insert (Line : in String; Before : in Positive)
   with
      Global => (In_Out => State),
      Pre    => Before in 1 .. Last + 1,
      Post   => Buffer.Line (Before) = Line;
   -- Adds Line to the buffer before line number Before

   procedure Delete (Number : in Positive)
   with
      Global => (In_Out => State),
      Pre    => Number in 1 .. Last,
      Post   => Last = Last'Old - 1;
   -- Deletes line number Number from the buffer

   procedure Replace (Number : in Positive; Line : in String)
   with
      Global => (In_Out => State),
      Pre    => Last < Integer'Last and then Number in 1 .. Last,
      Post   => Buffer.Line (Number) = Line;
   -- Makes line Number in the buffer have the value Line

   function Default_File return String with Global => (Input => State);
   -- Initially returns ""

   procedure Set_File (Name : in String)
   with
      Global => (In_Out => State),
      Pre    => Name /= "",
      Post   => Default_File = Name;

   procedure Load (File_Name : in String)
   with
      Global => (In_Out => State),
      Post   => Default_File = File_Name;
   -- Makes Default_File return File_Name
   -- Then, if a file named File_Name exists, clears the buffer, opens File_Name, reads it into the buffer, and closes it
   -- Otherwise, has no effect on the buffer

   procedure Write (File_Name : in String := "") with Global => (Input => State);
   -- If File_Name /= "", creates File_Name, writes the lines in the buffer to it, and closes it
   -- If File_Name = "" and Default_File /= "", creates Default_File, writes the lines in the buffer to it, and closes it
   -- If File_Name = "" and Default_File = "", has no effect

   function Last return Natural
   with
      Global => (Input => State),
      Post   => Last'Result < Integer'Last;
   -- Returns the number of the last line in the buffer = number of lines in the buffer
   -- Last = 0 means the buffer is empty

   function Line (Number : Positive) return String
   with
      Global => (Input => State),
      Pre    => Number in 1 .. Last;
   -- Returns line number Number in the buffer

   function Next (Number : Natural) return Positive
   with
      Global => (Input => State),
      Post   => (if Number < Last then Next'Result = Number + 1 else Next'Result = 1);
   -- Increments line number Number with wrap around

   function Prev (Number : Natural) return Positive
   with
      Global => (Input => State),
      Pre    => Number in 0 .. Last,
      Post   => (if Number > 1 then Prev'Result = Number - 1 else Prev'Result = Last);
   -- Decrements line number Number with wrap around
end Lined.Buffer;
