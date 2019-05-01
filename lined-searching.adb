with Ada.Strings.Unbounded;

with Lined.Buffer;

package body Lined.Searching with Refined_State => (State => (Cur_Pat, Processed) ) is
   use Ada.Strings.Unbounded;

   Cur_Pat   : Unbounded_String;
   Processed : PragmARC.Character_Regular_Expression_Matcher.Processed_Pattern;

   function Terminator (Pattern : String; Delimiter : Character; Classes : Boolean := True) return Positive is
      In_Class : Boolean := False;

      use PragmARC.Character_Regular_Expression_Matcher;
   begin -- Terminator
      Scan : for I in Pattern'Range loop
         if Pattern (I) = Delimiter and not In_Class and (I = Pattern'First or else Pattern (I - 1) /= Escape_Item) then
            return I;
         end if;

         case Pattern (I) is
         when Start_Class_Item =>
            if Classes and not In_Class then
               In_Class := True;
            end if;
         when Stop_Class_Item =>
            if Classes and In_Class then
               In_Class := False;
            end if;
         when others =>
            null;
         end case;
      end loop Scan;

      raise Invalid_Input; -- Delimiter not found
   end Terminator;

   procedure Process (Pattern : in String) is
      -- Empty
   begin -- Process
      Cur_Pat := To_Unbounded_String (Pattern);

      Handle_Invalid : declare
         -- Empty
      begin -- Handle_Invalid
         PragmARC.Character_Regular_Expression_Matcher.Process (Pattern => Pattern, Processed => Processed);
      exception -- Handle_Invalid
      when others =>
         Cur_Pat := Null_Unbounded_String;

         raise Invalid_Input;
      end Handle_Invalid;
   end Process;

   function Current return String is (To_String (Cur_Pat) );

   function Search (Current : Natural; Forward : Boolean) return Positive is
      Line : Natural := Current;
   begin -- Search
      if Buffer.Last = 0 or Cur_Pat = Null_Unbounded_String then
         raise Invalid_Input;
      end if;

      Match : loop
         if Forward then
            Line := Buffer.Next (Line);
         else
            Line := Buffer.Prev (Line);
         end if;

         if PragmARC.Character_Regular_Expression_Matcher.Location (Processed, Buffer.Line (Line) ).Found then
            return Line;
         end if;

         exit Match when Line = Current or (Current = 0 and Line = (if Forward then 1 else Buffer.Last) );
      end loop Match;

      raise Invalid_Input;
   end Search;

   function Search (Line : String) return PragmARC.Character_Regular_Expression_Matcher.Result is
      -- Empty
   begin -- Search
      if Cur_Pat = Null_Unbounded_String then
         raise Invalid_Input;
      end if;

      return PragmARC.Character_Regular_Expression_Matcher.Location (Processed, Line);
   end Search;
end Lined.Searching;
