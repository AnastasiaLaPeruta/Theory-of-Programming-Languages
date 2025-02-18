program CaesarCipher;

(* global variable declaration*)
var
   testString: string = "HAL"; 
   procString: string;
   atoz: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
   stringShift: integer = 2;
   maxShift: integer = 26;


(*encrypt is a function because it returns a single value*)
function encrypt(s: string; shift: integer): string;

var
   (* local variable declaration *)
   result: string = "";
   i: integer;
   charPos: integer;
   index: integer;
   ch: string;
   
begin

   for i := 1 to length(s) do
   begin
      ch := copy(s, i, 1);
      ch := UpperCase(ch);
      charPos := Pos(ch, atoz);
      if charPos = 0 then
         index := charPos + shift;
         if index > 26 then
            index := index - 26
            (*extracts char from string; shown command copy by ChatGPT*)
            result := result + Copy(atoz, index, 1)
         else
            result := result + ch;
      i = i + 1;
      return result;
    end;
end;




(*decrypt is a function because it returns a single value*)
function decrypt(s: string; shift: integer): string;

var
   (* local variable declaration *)
   example: integer;

begin

end;




(*solve is a procedure because it does not return a value directly*)
procedure solve(s: string; maxLabel: integer);
begin

end;




(*main program block where calls functions*)
begin
  writeln ('Hello, World!')
end.