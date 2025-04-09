%  ChatGPT assisted throughout
-module(main).
-export([main/1, to_uppercase/1, caesar_encrypt/2, caesar_decrypt/2,
          solve_caesar/1, test/0, test_result/0]).

%% Convert any input to uppercase
to_uppercase(String) ->
    lists:map(fun(Char) ->
        case Char of
            $a -> $A;
            $b -> $B;
            $c -> $C;
            $d -> $D;
            $e -> $E;
            $f -> $F;
            $g -> $G;
            $h -> $H;
            $i -> $I;
            $j -> $J;
            $k -> $K;
            $l -> $L;
            $m -> $M;
            $n -> $N;
            $o -> $O;
            $p -> $P;
            $q -> $Q;
            $r -> $R;
            $s -> $S;
            $t -> $T;
            $u -> $U;
            $v -> $V;
            $w -> $W;
            $x -> $X;
            $y -> $Y;
            $z -> $Z;
            _  -> Char
        end
    end, String).

%% Encrypts a string using a Caesar cipher in a functional manner.
caesar_encrypt(String, Shift) ->
    lists:map(fun(Char) ->
        if
            Char >= $A, Char =< $Z ->
                $A + ((Char - $A + Shift) rem 26);
            true ->
                Char
        end
    end, String).

%% Decrypts a string using a Caesar cipher in a functional manner.
caesar_decrypt(String, Shift) ->
    lists:map(fun(Char) ->
        if
            Char >= $A, Char =< $Z ->
                $A + ((Char - $A - Shift + 26) rem 26);
            true ->
                Char
        end
    end, String).

%% Solves (breaks) a Caesar cipher, shifts 26 to 0
solve_caesar(String) ->
    lists:map(fun(Shift) ->
        {Shift, caesar_encrypt(String, Shift)}
    end, lists:reverse(lists:seq(0,26))).

%% Test function
test() ->
    Orig = "TeSt",
    Shift = 8,
    UpperOrig = to_uppercase(Orig), 
    Encrypted = caesar_encrypt(UpperOrig, Shift), 
    Decrypted = caesar_decrypt(Encrypted, Shift), 
    Solved = solve_caesar(UpperOrig),
    {Orig, UpperOrig, Encrypted, Decrypted, Solved}.

%% Run the test 
test_result() ->
    {Orig, UpperOrig, Encrypted, Decrypted, Solved} = test(),
    io:format("Original: ~s~n", [Orig]),
    io:format("Encrypted: ~s~n", [Encrypted]),
    io:format("Decrypted: ~s~n~n", [Decrypted]),
    io:format("SOLVE for \"~s\":~n~n", [Orig]),
    lists:foreach(fun({Shift, Candidate}) ->
                          io:format("Caesar ~w: ~s~n", [Shift, Candidate])
                  end, Solved).


main(_Args) ->
    test_result().
