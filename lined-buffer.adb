with Ada.Containers.Indefinite_Vectors;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body Lined.Buffer with SPARK_Mode => Off, Refined_State => (State => (Line_List, File_Name) )
is
   type Line_Info (Length : Natural) is record
      Mark : Boolean := False;
      Line : String (1 .. Length);
   end record;

   package Line_Lists is new Ada.Containers.Indefinite_Vectors (Index_Type => Positive, Element_Type => Line_Info);

   Line_List : Line_Lists.Vector;
   File_Name : Ada.Strings.Unbounded.Unbounded_String;

   procedure Clear is
      -- Empty
   begin -- Clear
      Line_List.Clear;
   end Clear;

   procedure Mark (Number : in Positive; Status : in Boolean) is
      procedure Update (Item : in out Line_Info);
      -- Sets Item.Mark to Status

      procedure Update (Item : in out Line_Info) is
         -- Empty
      begin -- Update
         Item.Mark := Status;
      end Update;
   begin -- Mark
      Line_List.Update_Element (Index => Number, Process => Update'Access);
   end Mark;

   function Marked (Number : Positive) return Boolean is (Line_List.Element (Number).Mark);

   procedure Clear_Marks is
      -- Empty
   begin -- Clear_Marks
      All_Lines : for I in 1 .. Line_List.Last_Index loop
         Mark (Number => I, Status => False);
      end loop All_Lines;
   end Clear_Marks;

   procedure Insert (Line : in String; Before : in Positive) is
      -- Empty
   begin -- Insert
      Line_List.Insert (Before => Before, New_Item => (Length => Line'Length, Mark => False, Line => Line) );
   end Insert;

   procedure Delete (Number : in Positive) is
      -- Empty
   begin -- Delete
      Line_List.Delete (Index => Number);
   end Delete;

   procedure Replace (Number : in Positive; Line : in String) is
      -- Empty
   begin -- Replace
      Line_List.Replace_Element
         (Index => Number, New_Item => (Length => Line'Length, Mark => Line_List.Element (Number).Mark, Line => Line) );
   end Replace;

   use Ada.Strings.Unbounded;

   function Default_File return String is (To_String (File_Name) );

   procedure Set_File (Name : in String) is
      -- Empty
   begin -- Set_File
      File_Name := To_Unbounded_String (Name);
   end Set_File;

   procedure Load (File_Name : in String) is
      File : Ada.Text_IO.File_Type;
   begin -- Load
      Buffer.File_Name := To_Unbounded_String (File_Name);

      if not Ada.Directories.Exists (File_Name) then
         return;
      end if;

      Clear;
      Ada.Text_IO.Open (File => File, Mode => Ada.Text_IO.In_File, Name => File_Name);

      Handle_EOF : declare
         -- Empty
      begin -- Handle_EOF
         All_Lines : loop
            exit All_Lines when Ada.Text_IO.End_Of_File (File);

            Insert (Line => Ada.Text_IO.Get_Line (File), Before => Last + 1);
         end loop All_Lines;
      exception -- Handle_EOF
      when Ada.Text_IO.End_Error =>
         null;
      end Handle_EOF;

      Ada.Text_IO.Close (File => File);
   end Load;

   procedure Write (File_Name : in String := "") is
      File : Ada.Text_IO.File_Type;
   begin -- Write
      if File_Name = "" and Buffer.File_Name = Null_Unbounded_String then
         return;
      end if;

      if File_Name = "" then
         Ada.Text_IO.Create (File => File, Name => To_String (Buffer.File_Name) );
      else
         Ada.Text_IO.Create (File => File, Name => File_Name);
         Buffer.File_Name := To_Unbounded_String (File_Name);
      end if;

      All_Lines : for I in 1 .. Last loop
         Ada.Text_IO.Put_Line (File => File, Item => Line (I) );
      end loop All_Lines;

      Ada.Text_IO.Close (File => File);
   end Write;

   function Last return Natural is (Line_List.Last_Index);

   function Line (Number : Positive) return String is (Line_List.Element (Number).Line);

   function Next (Number : Natural) return Positive is (if Number >= Last then 1 else Number + 1);

   function Prev (Number : Natural) return Positive is (if Number < 2 then Last else Number - 1);
end Lined.Buffer;
