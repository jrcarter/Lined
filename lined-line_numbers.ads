-- Each command may be preceded by line numbers that indicate the lines to operate on
-- There is also the concept of the current line number ("dot")
-- Line_Numbers parses commands to extract line numbers into Start and Stop
--
-- A line number is made up of one or more components, which can be
-- <number>    a non-negative integer representing an absolute line number
-- .           dot, the current line number
-- $           the last line
-- /<pattern>/ forward search: the first line after the current line that contains <pattern>, a regular expression; the search wraps
-- \<pattern>\ backward search: the first line before the current line that contains <pattern>; the search wraps
-- The last pattern specified is remembered and returned by Searching.Current. The shortcuts // and \\ mean to search for the saved
-- pattern
--
-- Nultiple components are separated by the mathematical operators + or -; the line number is the mathematical result of applying
-- these operators to the components
-- .+1         the line after the current line
-- $-10        the 10th line before the last line
--
-- Multiple line numbers are separated by a comma (,) or semicolon (;)
-- A line number followed by a semicolon sets the current line number to the given line number, thus affecting the use of dot in
-- following line numbers;
-- a line number followed by a comma does not affect the current line number
-- /abc/;//;// the 3rd following occurence of "abc"

with Lined.Buffer;
with Lined.Searching;

package Lined.Line_Numbers with SPARK_Mode, Abstract_State => State, Initializes => State is
   procedure Get_Line_Number (Source : in String; Current : in Natural; Last : out Natural; Value : out Natural)
   with Pre => Source'Length > 0 and Source'Last < Integer'Last;
   -- Gets a full line number (sum/difference of components) from Source
   -- Current is the current line number
   -- The last position in Source that is part of the line number is returned in Last
   -- Value is the line number obtained
   -- Last < Source'First if Source doesn't begin with a line number

   procedure Parse (Command : in String; Current : in out Natural; Last : out Natural)
   with
      Global => (Output => State),
      Pre    => Command'Length > 0 and Command'Last < Integer'Last;
   -- Parses line numbers from the beginning of Command; Current is the current line number
   -- Last is set to the index in Command of the last character included in a line number; if there are no line numbers, Last = 0
   -- Sets the results of Num_Numbers, Start, and Stop (and Searching.Current)
   -- If Command has line numbers that are search patterns, Searching.Current will return the last pattern encountered
   -- If Command has no line numbers, Num_Numbers will return 0; Start and Stop will be set to the current line number, and
   -- Searching.Current is unchanged
   -- If Command has a single line number, Num_Numbers will return 1; Start and Stop will return that line number;
   --    Current is unchanged
   -- If Command has 2 line numbers, Num_Numbers will return 2; Start will return the first number; Stop will return the second
   --    number; Current will be changed if they are spearated by a semicolon
   -- If Command has > 2 line numbers, Num_Numbers will return 2; Start will return the next to last number; Stop will return the
   --    last number; Current will be changed if any of the line number separators are semicolons

   subtype Number_Count is Integer range 0 .. 2;

   function Num_Numbers return Number_Count With Global => (Input => State);

   function Start return Natural With Global => (Input => State);

   function Stop return Natural With Global => (Input => State);
   -- These functions initially return 0
end Lined.Line_Numbers;
