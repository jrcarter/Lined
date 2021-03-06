with PragmARC.Character_Regular_Expression_Matcher;

package Lined.Searching with Abstract_State => State, Initializes => State is
   function Terminator (Pattern : String; Delimiter : Character; Classes : Boolean := True) return Positive;
   -- Returns the index in Pattern of the first occurence of Delimiter that is not preceded by an escape
   -- (PragmARC.Character_Regular_Expression_Matcher.Escape_Item) or, if Classes, in a class ('[', ']')
   -- Classes of False is useful for finding the terminator of a replacement string
   -- Raises Invalid_Input if Pattern does not contain such an occurence of Delimiter

   procedure Process (Pattern : in String) with
      Global => (In_Out => State),
      Post   => Pattern = Current;
   -- Processes Pattern for future searches and stores Pattern for calls to Search
   -- Makes Current return Pattern
   -- Raises Invalid_Input if Pattern is not a valid pattern; Current will return "" in this case

   function Current return String with
      Global => State;
   -- Returns the pattern most recently processed by Process
   -- Returns "" if Process raised Invalid_Input
   -- Initially returns ""

   function Search (Current : Natural; Forward : Boolean) return Positive with
      Global => State;
   -- Returns the number of the next (if Forward) or previous (if not Forward) line matching Searching.Current
   -- Raises Invalid_Input if no line matches

   function Search (Line : String) return PragmARC.Character_Regular_Expression_Matcher.Result with
      Global => State;
   -- Returns PragmARC.Character_Regular_Expression_Matcher.Location (Current, Line)
end Lined.Searching;
