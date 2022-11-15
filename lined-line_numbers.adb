with Ada.Strings.Fixed;
with Ada.Strings.Maps;

with Lined.Buffer;
with Lined.Searching;

package body Lined.Line_Numbers is
   Num_Found : Number_Count := 0;
   Line1     : Natural      := 0; -- Start of range if multiple line numbers are given
   Line2     : Natural      := 0; -- End of range if multiple line numbers are given; line to operate on if a single number is given

   procedure Get_Line_Number (Source : in String; Current : in Natural; Last : out Natural; Value : out Natural) is
      subtype Digit is Character range '0' .. '9';

      Forward : constant Character := '/';

      Digit_Set  : constant Ada.Strings.Maps.Character_Set :=
         Ada.Strings.Maps.To_Set (Span => (Low => Digit'First, High => Digit'Last) );

      procedure Get_Number (Source : in String; Last : out Natural; Value : out Natural) with Pre => Source'Length > 0;
      -- Gets a Component from Source
      -- Last is set to the index of the last Character in the component
      -- Last < Source'First if Source doesn't begin with a component

      procedure Get_Number (Source : in String; Last : out Natural; Value : out Natural) is
         Final : Natural;
      begin -- Get_Number
         case Source (Source'First) is
         when '.' =>
            Value := Current;
            Last := Source'First;
         when '$' =>
            Value := Buffer.Last;
            Last := Source'First;
         when Forward | '\' =>
            Final := Searching.Terminator (Source (Source'First + 1 .. Source'Last), Source (Source'First) );
            Last := Final;

            if Final > Source'First + 1 then -- New pattern
               Searching.Process (Pattern => Source (Source'First + 1 .. Final - 1) );
            end if;

            Value := Searching.Search (Current => Current, Forward => Source (Source'First) = Forward);
         when Digit =>
            Final := Ada.Strings.Fixed.Index (Source, Digit_Set, Source'First + 1, Ada.Strings.Outside);

            if Final = 0 then
               Final := Source'Last;
            else
               Final := Final - 1;
            end if;

            Value := Integer'Value (Source (Source'First .. Final) );
            Last := Final;
         when others =>
            Value := 0;
            Last := Source'First - 1;
         end case;
      end Get_Number;

      Plus : constant Character := '+';

      Index : Natural := Ada.Strings.Fixed.Index_Non_Blank (Source);
      Final : Natural;
      Comp  : Natural;
      Sign  : Integer;
   begin -- Get_Line_Number
      Last := 0;
      Get_Number (Source => Source (Index .. Source'Last), Last => Final, Value => Value);

      if Final < Index then -- Not a component
         return;
      end if;

      All_Components : loop -- Any additional components separated by +/-
         Index := Ada.Strings.Fixed.Index_Non_Blank (Source (Final + 1 .. Source'Last) );

         exit All_Components when Index not in Source'Range or else Source (Index) not in Plus | '-'; -- No more components

         if Source (Index) = Plus then
            Sign := +1;
         else
            Sign := -1;
         end if;

         Index := Ada.Strings.Fixed.Index_Non_Blank (Source (Index + 1 .. Source'Last) );

         if Index not in Source'Range then
            raise Invalid_Input; -- +/- not followed by anything
         end if;

         Get_Number (Source => Source (Index .. Source'Last), Last => Final, Value => Comp);

         if Final < Index then
            raise Invalid_Input; -- +/- followed by non-component
         end if;

         Value := Value + Sign * Comp;
      end loop All_Components;

      if Value > Buffer.Last then
         raise Invalid_Input;
      end if;

      Last := (if Index = 0 then Source'Last else Index - 1);
   exception -- Get_Line_Number
   when others => -- Typically a range check failed
      raise Invalid_Input;
   end Get_Line_Number;

   procedure Parse (Command : in String; Current : in out Natural; Last : out Natural) is
      Semicolon : constant Character := ';';

      Index : Natural := Command'First;
      Final : Natural;
   begin -- Parse
      Num_Found := 0;
      Line1     := 0;
      Line2     := 0;

      All_Numbers : loop
         exit All_Numbers when Index not in Command'Range;

         Line1 := Line2;
         Get_Line_Number (Source => Command (Index .. Command'Last), Current => Current, Last => Final, Value => Line2);

         exit All_Numbers when Final < Index;

         pragma Assert (Final <= Command'Last);

         Num_Found := Integer'Min (Num_Found + 1, Number_Count'Last);
         Index := Ada.Strings.Fixed.Index_Non_Blank (Command (Final + 1 .. Command'Last) );

         exit All_Numbers when Index not in Command'Range or else Command (Index) not in ',' | Semicolon;

         if Command (Index) = Semicolon then
            Current := Line2;
         end if;

         Index := Index + 1;
      end loop All_Numbers;

      if Num_Found = 0 then
         Line2 := Current;
      end if;

      Last := (if Index = 0 then Command'Last else Index - 1);

      if Num_Found < 2 then
         Line1 := Line2;
      end if;
   end Parse;

   function Num_Numbers return Number_Count is (Num_Found);

   function Start return Natural is (Line1);

   function Stop return Natural is (Line2);
end Lined.Line_Numbers;
