(* ChatGPT assisted throughout *)

(* Convert any input to uppercase *)
fun charToUpper c =
  if c >= #"a" andalso c <= #"z" then
    Char.chr (Char.ord c - 32)
  else
    c;

fun to_upper text =
  String.implode (List.map charToUpper (String.explode text));

(* Encrypts a string using a Caesar cipher in a functional manner *)
fun caesar_encrypt (text: string, shift: int) =
  let
    val text = to_upper text  (* Convert the input to uppercase. *)
    fun encrypt_char c =
      if Char.isAlpha c then
        let
          val base = Char.ord #"A"
          val offset = Char.ord c - base
          val new_offset = (offset + shift) mod 26
        in
          Char.chr (base + new_offset)
        end
      else c
  in
    String.map encrypt_char text
  end;

(* Decrypts a string using a Caesar cipher in a functional manner *)
fun caesar_decrypt (text: string, shift: int) =
  let
    fun decrypt_char c =
      if Char.isAlpha c then
        let
          val base = Char.ord #"A"
          val offset = Char.ord c - base
          val new_offset = (offset - shift + 26) mod 26
        in
          Char.chr (base + new_offset)
        end
      else c
  in
    String.map decrypt_char text
  end;

(* Solves (breaks) a Caesar cipher by printing encryptions for each possible shift *)
fun solve (text: string, max_shift: int) =
  let
    fun solve_rec s =
      if s >= 0 then
        let
          val encrypted = caesar_encrypt (text, s)
        in
          (print (String.concat ["Caesar ", Int.toString s, ": ", encrypted, "\n"]);
           solve_rec (s - 1))
        end
      else ()
  in
    solve_rec max_shift
  end;

(* Test function *)
fun test_caesar_cipher () =
  let
    val original = "TeSt"
    val shift = 8
    val encrypted = caesar_encrypt (original, shift)
    val decrypted = caesar_decrypt (encrypted, shift)
  in
    print (String.concat ["Original: ", original, "\n"]);
    print (String.concat ["Encrypted: ", encrypted, "\n"]);
    print (String.concat ["Decrypted: ", decrypted, "\n"]);
    print "\nSOLVE for \"TeSt\":\n";
    solve ("TeSt", 26)
  end;

(* Run the test *)
val _ = test_caesar_cipher ();
