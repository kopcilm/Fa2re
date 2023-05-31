%true iff there is even number of 'a' or even number of 'b' 
mat1(String):- 
    string_chars(String, Chars),
    automaton(
        Chars, 
        [
            ['0', lambda(), '1'],
            ['0', lambda(), '3'],
            ['1', a, '2'],
            ['1', b, '1'],
            ['2', a, '1'],
            ['2', b, '2'],
            ['3', a, '3'],
            ['3', b, '4'],
            ['4', a, '4'],
            ['4', b, '3']
        ],
        ['0'],
        ['1', '3']
    ).

%true iff there is even number of 'a' or even number of 'b'     
reg1(String):- 
    string_chars(String, Chars),
    regularExpression(
        Chars,
        (a*)
        +(((a*):b:(a*):b:(a*))*)
        +(b*)
        +(((b*):a:(b*):a:(b*))*)
    ).
