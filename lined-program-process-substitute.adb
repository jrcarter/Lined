with PragmARC.Matching.Character_Regular_Expression;

separate (Lined.Program.Process)
procedure Substitute (Command : in String; Current : in out Natural; Last : out Natural; Printing : out Boolean) is
   procedure Replace (Replacement : in String; Multi : in Boolean; Start : in Natural; Stop : in Natural; Current : in out Natural)
   with
      Pre => Start <= Stop and Stop <= Buffer.Last;
   -- Searches lines Start .. Stop for the current pattern
   -- If the pattern is found in a line, replaces it with Replacement, with any non-escaped instances of '@' replaced by the
   -- matched part of the line; if Multi, then repeats this for any other matches on the line
   -- Raises Invalid_Input if no match is found

   procedure Replace (Replacement : in String; Multi : in Boolean; Start : in Natural; Stop : in Natural; Current : in out Natural)
   is
      use PragmARC;

      function Expanded_Replacment (Line : in String; First : in Positive := Replacement'First) return String;
      -- Returns Replacement (First .. Replacement'Last), dealing with escaped characters and replacing '@' with Line

      procedure Replace_Rest (Head : in String; Line : in String; Number : in Positive) with
         Pre => Number in 1 .. Buffer.Last;
      -- Replaces buffer line Number with Head followed by Line with matches replaced by Replacement

      function Expanded_Replacment (Line : in String; First : in Positive := Replacement'First) return String is
         -- Empty
      begin -- Expanded_Replacment
         if First > Replacement'Last then
            return "";
         end if;

         case Replacement (First) is
         when Matching.Character_Regular_Expression.Escape_Item =>
            if First = Replacement'Last then
               return Replacement (First .. First);
            end if;

            return Replacement (First + 1) & Expanded_Replacment (Line, First + 2);
         when '@' =>
            return Line & Expanded_Replacment (Line, First + 1);
         when others =>
            return Replacement (First) & Expanded_Replacment (Line, First + 1);
         end case;
      end Expanded_Replacment;

      procedure Replace_Rest (Head : in String; Line : in String; Number : in Positive) is
         Result : constant Matching.Character_Regular_Expression.Result := Searching.Search (Line);
      begin -- Replace_Rest
         if not Result.Found then
            Buffer.Replace (Number => Number, Line => Head & Line);

            return;
         end if;

         Replace_Rest (Head   => Head &
                                 Line (Line'First .. Result.Start - 1) &
                                 Expanded_Replacment (Line (Result.Start .. Result.Start + Result.Length - 1) ),
                       Line   => Line (Result.Start + Result.Length .. Line'Last),
                       Number => Number);
      end Replace_Rest;

      Matched : Boolean := False;
   begin -- Replace
      All_Lines : for L in Start .. Stop loop
         One_Line : declare
            Line   : constant String                                       := Buffer.Line (L);
            First  : constant Natural                                      := Line'First;
            Result : constant Matching.Character_Regular_Expression.Result := Searching.Search (Line);
         begin -- One_Line
            if Result.Found then
               Matched := True;

               if Multi then
                  Replace_Rest
                     (Head   => Line (First .. Result.Start - 1) &
                                Expanded_Replacment (Line => Line (Result.Start .. Result.Start + Result.Length - 1) ),
                      Line   => Line (Result.Start + Result.Length .. Line'Last),
                      Number => L);
               else
                  Buffer.Replace
                     (Number => L,
                      Line   => Line (First .. Result.Start - 1) &
                         Expanded_Replacment (Line => Line (Result.Start .. Result.Start + Result.Length - 1) ) &
                         Line (Result.Start + Result.Length .. Line'Last) );
               end if;
            end if;
         end One_Line;
      end loop All_Lines;

      if not Matched and not Global then
         raise Invalid_Input;
      end if;

      Current := Stop;
   end Replace;

   First : Natural := Ada.Strings.Fixed.Index_Non_Blank (Command);
   Index : Natural;
   Multi : Boolean := False;
   Start : Natural := Line_Numbers.Start;
   Stop  : Natural := Line_Numbers.Stop;
begin -- Substitute
   Last := 0;
   Printing := False;

   if First = 0 then
      raise Invalid_Input;
   end if;

   Last := Searching.Terminator (Command (First + 1 .. Command'Last), Command (First) );

   if Last > First + 1 then -- New pattern
      Searching.Process (Pattern => Command (First + 1 .. Last - 1) );
   end if;

   First := Last;
   Last := Searching.Terminator (Command (First + 1 .. Command'Last), Command (First), False);
   Index := Ada.Strings.Fixed.Index_Non_Blank (Command (Last + 1 .. Command'Last) );

   if Index /= 0 then
      Multi := Command (Index) = Global_Indicator;
      Printing := Printing_Requested (Command (Index + (if Multi then 1 else 0) .. Command'Last) );
   end if;

   Set_Lines (Default_Start => Current, Default_Stop => Current, Start => Start, Stop => Stop);
   Replace (Replacement => Command (First + 1 .. Last - 1), Multi => Multi, Start => Start, Stop => Stop, Current => Current);
end Substitute;
