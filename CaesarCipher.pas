program CaesarCipher;

uses
  SysUtils; (* so I can use Trim function *)

(* global variable declaration *)
var
   testString: string = 'HAL'; 
   procString: string;
   atoz: string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
   stringShift: integer = 2;
   maxShift: integer = 26;
   encrypted, decrypted: string;

(* encrypt is a function because it returns a single value *)
function encrypt(s: string; shift: integer): string;
var
   (* local variable declaration *)
   result: string = '';
   i: integer;
   charPos: integer;
   index: integer;
   ch: string;
begin
   for i := 1 to length(s) do
   begin
      ch := Copy(s, i, 1);
      ch := UpCase(ch);
      charPos := Pos(ch, atoz);
      if charPos <> 0 then
      begin
         index := charPos + shift;
         if index > 26 then
            index := index - 26;
         (* extracts char from string; shown command copy by ChatGPT *)
         result := result + Copy(atoz, index, 1);
      end
      else
         result := result + ch;
   end;
   encrypt := result;
end;

(* decrypt is a function because it returns a single value *)
function decrypt(s: string; shift: integer): string;
var
   (* local variable declaration *)
   result: string = '';
   i: integer;
   charPos: integer;
   index: integer;
   ch: string;
begin
   for i := 1 to length(s) do
   begin
      ch := Copy(s, i, 1);
      charPos := Pos(ch, atoz);
      if charPos <> 0 then
      begin
         index := charPos - shift;
         if index < 1 then
            index := index + 26;
         result := result + Copy(atoz, index, 1);
      end
      else
         result := result + ch;
   end;
   decrypt := result; 
end;

(* solve is a procedure because it does not return a value directly *)
procedure solve(s: string; maxLabel: integer);
var
   (* local variable declaration *)
   L, currentShift: integer;
begin
   (* ChatGPT helped translate these lines from my BASIC program *)
   for L := maxLabel downto 0 do
   begin
      currentShift := (stringShift + (26 - L)) mod 26;
      writeln('Caesar ', L, ': ', decrypt(s, currentShift));
   end;  { end for loop }
end;  { end procedure solve }

(* main program block where calls functions *)
begin
  (* makes sure string is capitalized and has no white space attached on ends *)
  procString := UpCase(testString);
  procString := Trim(procString);
  writeln('Test String: ', testString);
  encrypted := encrypt(procString, stringShift);
  writeln('Encrypted: ', encrypted);
  decrypted := decrypt(encrypted, stringShift);
  writeln('Decrypted: ', decrypted);
  writeln;
  writeln('Trying all shifts:');
  solve(encrypted, maxShift);
  writeln;
end.
