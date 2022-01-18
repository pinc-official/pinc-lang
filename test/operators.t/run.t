  $ esy x print ./data.fe Section
  <div>
  ![1] = false
  ![] = true
  !!"" = false
  !true = false
  -1 = -1
  -2.5 = -2.5
  
  true == true = true
  true != true = false
  true && true = true
  true && false = false
  true || false = true
  false || false = false
  
  1 > 2 = false
  1 < 2 = true
  1 <= 1 = true
  2 <= 1 = false
  1 >= 1 = true
  
  [1] == [1] = true
  [1] == [2] = false
  [1, 2] >= [5] = true
  [1] >= [1,2,3] = false
  </div>


  $ esy x print_tokens ./data.fe
  { typ = KEYWORD_COMPONENT;
    start_pos = { filename = "./data.fe"; line = 1; column = 1 };
    end_pos = { filename = "./data.fe"; line = 1; column = 10 } }
  { typ = (IDENT_UPPER "Operators");
    start_pos = { filename = "./data.fe"; line = 1; column = 11 };
    end_pos = { filename = "./data.fe"; line = 1; column = 20 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 1; column = 21 };
    end_pos = { filename = "./data.fe"; line = 1; column = 22 } }
  { typ = TEMPLATE;
    start_pos = { filename = "./data.fe"; line = 2; column = 3 };
    end_pos = { filename = "./data.fe"; line = 2; column = 12 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 2; column = 13 };
    end_pos = { filename = "./data.fe"; line = 2; column = 14 } }
  { typ = (HTML_OPEN_TAG "div");
    start_pos = { filename = "./data.fe"; line = 3; column = 5 };
    end_pos = { filename = "./data.fe"; line = 3; column = 9 } }
  { typ = GREATER;
    start_pos = { filename = "./data.fe"; line = 3; column = 9 };
    end_pos = { filename = "./data.fe"; line = 3; column = 10 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 3; column = 10 };
    end_pos = { filename = "./data.fe"; line = 3; column = 11 } }
  { typ = (STRING "\n");
    start_pos = { filename = "./data.fe"; line = 3; column = 11 };
    end_pos = { filename = "./data.fe"; line = 3; column = 15 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 3; column = 15 };
    end_pos = { filename = "./data.fe"; line = 3; column = 16 } }
  { typ = (STRING "![1] = ");
    start_pos = { filename = "./data.fe"; line = 4; column = 7 };
    end_pos = { filename = "./data.fe"; line = 4; column = 14 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 4; column = 14 };
    end_pos = { filename = "./data.fe"; line = 4; column = 15 } }
  { typ = NOT; start_pos = { filename = "./data.fe"; line = 4; column = 15 };
    end_pos = { filename = "./data.fe"; line = 4; column = 16 } }
  { typ = LEFT_BRACK;
    start_pos = { filename = "./data.fe"; line = 4; column = 16 };
    end_pos = { filename = "./data.fe"; line = 4; column = 17 } }
  { typ = (INT 1);
    start_pos = { filename = "./data.fe"; line = 4; column = 17 };
    end_pos = { filename = "./data.fe"; line = 4; column = 18 } }
  { typ = RIGHT_BRACK;
    start_pos = { filename = "./data.fe"; line = 4; column = 18 };
    end_pos = { filename = "./data.fe"; line = 4; column = 19 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 4; column = 19 };
    end_pos = { filename = "./data.fe"; line = 4; column = 20 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 4; column = 21 };
    end_pos = { filename = "./data.fe"; line = 4; column = 22 } }
  { typ = (STRING "\n");
    start_pos = { filename = "./data.fe"; line = 4; column = 22 };
    end_pos = { filename = "./data.fe"; line = 4; column = 26 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 4; column = 26 };
    end_pos = { filename = "./data.fe"; line = 4; column = 27 } }
  { typ = (STRING "![] = ");
    start_pos = { filename = "./data.fe"; line = 5; column = 7 };
    end_pos = { filename = "./data.fe"; line = 5; column = 13 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 5; column = 13 };
    end_pos = { filename = "./data.fe"; line = 5; column = 14 } }
  { typ = NOT; start_pos = { filename = "./data.fe"; line = 5; column = 14 };
    end_pos = { filename = "./data.fe"; line = 5; column = 15 } }
  { typ = LEFT_BRACK;
    start_pos = { filename = "./data.fe"; line = 5; column = 15 };
    end_pos = { filename = "./data.fe"; line = 5; column = 16 } }
  { typ = RIGHT_BRACK;
    start_pos = { filename = "./data.fe"; line = 5; column = 16 };
    end_pos = { filename = "./data.fe"; line = 5; column = 17 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 5; column = 17 };
    end_pos = { filename = "./data.fe"; line = 5; column = 18 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 5; column = 19 };
    end_pos = { filename = "./data.fe"; line = 5; column = 20 } }
  { typ = (STRING "\n");
    start_pos = { filename = "./data.fe"; line = 5; column = 20 };
    end_pos = { filename = "./data.fe"; line = 5; column = 24 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 5; column = 24 };
    end_pos = { filename = "./data.fe"; line = 5; column = 25 } }
  { typ = (STRING "!!\"\" = ");
    start_pos = { filename = "./data.fe"; line = 6; column = 7 };
    end_pos = { filename = "./data.fe"; line = 6; column = 14 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 6; column = 14 };
    end_pos = { filename = "./data.fe"; line = 6; column = 15 } }
  { typ = NOT; start_pos = { filename = "./data.fe"; line = 6; column = 15 };
    end_pos = { filename = "./data.fe"; line = 6; column = 16 } }
  { typ = NOT; start_pos = { filename = "./data.fe"; line = 6; column = 16 };
    end_pos = { filename = "./data.fe"; line = 6; column = 17 } }
  { typ = (STRING "");
    start_pos = { filename = "./data.fe"; line = 6; column = 17 };
    end_pos = { filename = "./data.fe"; line = 6; column = 19 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 6; column = 19 };
    end_pos = { filename = "./data.fe"; line = 6; column = 20 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 6; column = 21 };
    end_pos = { filename = "./data.fe"; line = 6; column = 22 } }
  { typ = (STRING "\n");
    start_pos = { filename = "./data.fe"; line = 6; column = 22 };
    end_pos = { filename = "./data.fe"; line = 6; column = 26 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 6; column = 26 };
    end_pos = { filename = "./data.fe"; line = 6; column = 27 } }
  { typ = (STRING "!true = ");
    start_pos = { filename = "./data.fe"; line = 7; column = 7 };
    end_pos = { filename = "./data.fe"; line = 7; column = 15 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 7; column = 15 };
    end_pos = { filename = "./data.fe"; line = 7; column = 16 } }
  { typ = NOT; start_pos = { filename = "./data.fe"; line = 7; column = 16 };
    end_pos = { filename = "./data.fe"; line = 7; column = 17 } }
  { typ = KEYWORD_TRUE;
    start_pos = { filename = "./data.fe"; line = 7; column = 17 };
    end_pos = { filename = "./data.fe"; line = 7; column = 21 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 7; column = 21 };
    end_pos = { filename = "./data.fe"; line = 7; column = 22 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 7; column = 23 };
    end_pos = { filename = "./data.fe"; line = 7; column = 24 } }
  { typ = (STRING "\n");
    start_pos = { filename = "./data.fe"; line = 7; column = 24 };
    end_pos = { filename = "./data.fe"; line = 7; column = 28 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 7; column = 28 };
    end_pos = { filename = "./data.fe"; line = 7; column = 29 } }
  { typ = (STRING "-1 = ");
    start_pos = { filename = "./data.fe"; line = 8; column = 7 };
    end_pos = { filename = "./data.fe"; line = 8; column = 12 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 8; column = 12 };
    end_pos = { filename = "./data.fe"; line = 8; column = 13 } }
  { typ = UNARY_MINUS;
    start_pos = { filename = "./data.fe"; line = 8; column = 13 };
    end_pos = { filename = "./data.fe"; line = 8; column = 14 } }
  { typ = (INT 1);
    start_pos = { filename = "./data.fe"; line = 8; column = 14 };
    end_pos = { filename = "./data.fe"; line = 8; column = 15 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 8; column = 15 };
    end_pos = { filename = "./data.fe"; line = 8; column = 16 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 8; column = 17 };
    end_pos = { filename = "./data.fe"; line = 8; column = 18 } }
  { typ = (STRING "\n");
    start_pos = { filename = "./data.fe"; line = 8; column = 18 };
    end_pos = { filename = "./data.fe"; line = 8; column = 22 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 8; column = 22 };
    end_pos = { filename = "./data.fe"; line = 8; column = 23 } }
  { typ = (STRING "-2.5 = ");
    start_pos = { filename = "./data.fe"; line = 9; column = 7 };
    end_pos = { filename = "./data.fe"; line = 9; column = 14 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 9; column = 14 };
    end_pos = { filename = "./data.fe"; line = 9; column = 15 } }
  { typ = UNARY_MINUS;
    start_pos = { filename = "./data.fe"; line = 9; column = 15 };
    end_pos = { filename = "./data.fe"; line = 9; column = 16 } }
  { typ = (FLOAT 2.5);
    start_pos = { filename = "./data.fe"; line = 9; column = 16 };
    end_pos = { filename = "./data.fe"; line = 9; column = 19 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 9; column = 19 };
    end_pos = { filename = "./data.fe"; line = 9; column = 20 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 9; column = 21 };
    end_pos = { filename = "./data.fe"; line = 9; column = 22 } }
  { typ = (STRING "\n");
    start_pos = { filename = "./data.fe"; line = 9; column = 22 };
    end_pos = { filename = "./data.fe"; line = 9; column = 26 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 9; column = 26 };
    end_pos = { filename = "./data.fe"; line = 9; column = 27 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 10; column = 7 };
    end_pos = { filename = "./data.fe"; line = 10; column = 8 } }
  { typ = (STRING "\n");
    start_pos = { filename = "./data.fe"; line = 10; column = 8 };
    end_pos = { filename = "./data.fe"; line = 10; column = 12 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 10; column = 12 };
    end_pos = { filename = "./data.fe"; line = 10; column = 13 } }
  { typ = (STRING "true == true = ");
    start_pos = { filename = "./data.fe"; line = 11; column = 7 };
    end_pos = { filename = "./data.fe"; line = 11; column = 22 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 11; column = 22 };
    end_pos = { filename = "./data.fe"; line = 11; column = 23 } }
  { typ = KEYWORD_TRUE;
    start_pos = { filename = "./data.fe"; line = 11; column = 23 };
    end_pos = { filename = "./data.fe"; line = 11; column = 27 } }
  { typ = EQUAL_EQUAL;
    start_pos = { filename = "./data.fe"; line = 11; column = 28 };
    end_pos = { filename = "./data.fe"; line = 11; column = 30 } }
  { typ = KEYWORD_TRUE;
    start_pos = { filename = "./data.fe"; line = 11; column = 31 };
    end_pos = { filename = "./data.fe"; line = 11; column = 35 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 11; column = 35 };
    end_pos = { filename = "./data.fe"; line = 11; column = 36 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 11; column = 37 };
    end_pos = { filename = "./data.fe"; line = 11; column = 38 } }
  { typ = (STRING "\n");
    start_pos = { filename = "./data.fe"; line = 11; column = 38 };
    end_pos = { filename = "./data.fe"; line = 11; column = 42 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 11; column = 42 };
    end_pos = { filename = "./data.fe"; line = 11; column = 43 } }
  { typ = (STRING "true != true = ");
    start_pos = { filename = "./data.fe"; line = 12; column = 7 };
    end_pos = { filename = "./data.fe"; line = 12; column = 22 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 12; column = 22 };
    end_pos = { filename = "./data.fe"; line = 12; column = 23 } }
  { typ = KEYWORD_TRUE;
    start_pos = { filename = "./data.fe"; line = 12; column = 23 };
    end_pos = { filename = "./data.fe"; line = 12; column = 27 } }
  { typ = NOT_EQUAL;
    start_pos = { filename = "./data.fe"; line = 12; column = 28 };
    end_pos = { filename = "./data.fe"; line = 12; column = 30 } }
  { typ = KEYWORD_TRUE;
    start_pos = { filename = "./data.fe"; line = 12; column = 31 };
    end_pos = { filename = "./data.fe"; line = 12; column = 35 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 12; column = 35 };
    end_pos = { filename = "./data.fe"; line = 12; column = 36 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 12; column = 37 };
    end_pos = { filename = "./data.fe"; line = 12; column = 38 } }
  { typ = (STRING "\n");
    start_pos = { filename = "./data.fe"; line = 12; column = 38 };
    end_pos = { filename = "./data.fe"; line = 12; column = 42 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 12; column = 42 };
    end_pos = { filename = "./data.fe"; line = 12; column = 43 } }
  { typ = (STRING "true && true = ");
    start_pos = { filename = "./data.fe"; line = 13; column = 7 };
    end_pos = { filename = "./data.fe"; line = 13; column = 22 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 13; column = 22 };
    end_pos = { filename = "./data.fe"; line = 13; column = 23 } }
  { typ = KEYWORD_TRUE;
    start_pos = { filename = "./data.fe"; line = 13; column = 23 };
    end_pos = { filename = "./data.fe"; line = 13; column = 27 } }
  { typ = LOGICAL_AND;
    start_pos = { filename = "./data.fe"; line = 13; column = 28 };
    end_pos = { filename = "./data.fe"; line = 13; column = 30 } }
  { typ = KEYWORD_TRUE;
    start_pos = { filename = "./data.fe"; line = 13; column = 31 };
    end_pos = { filename = "./data.fe"; line = 13; column = 35 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 13; column = 35 };
    end_pos = { filename = "./data.fe"; line = 13; column = 36 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 13; column = 37 };
    end_pos = { filename = "./data.fe"; line = 13; column = 38 } }
  { typ = (STRING "\n");
    start_pos = { filename = "./data.fe"; line = 13; column = 38 };
    end_pos = { filename = "./data.fe"; line = 13; column = 42 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 13; column = 42 };
    end_pos = { filename = "./data.fe"; line = 13; column = 43 } }
  { typ = (STRING "true && false = ");
    start_pos = { filename = "./data.fe"; line = 14; column = 7 };
    end_pos = { filename = "./data.fe"; line = 14; column = 23 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 14; column = 23 };
    end_pos = { filename = "./data.fe"; line = 14; column = 24 } }
  { typ = KEYWORD_TRUE;
    start_pos = { filename = "./data.fe"; line = 14; column = 24 };
    end_pos = { filename = "./data.fe"; line = 14; column = 28 } }
  { typ = LOGICAL_AND;
    start_pos = { filename = "./data.fe"; line = 14; column = 29 };
    end_pos = { filename = "./data.fe"; line = 14; column = 31 } }
  { typ = KEYWORD_FALSE;
    start_pos = { filename = "./data.fe"; line = 14; column = 32 };
    end_pos = { filename = "./data.fe"; line = 14; column = 37 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 14; column = 37 };
    end_pos = { filename = "./data.fe"; line = 14; column = 38 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 14; column = 39 };
    end_pos = { filename = "./data.fe"; line = 14; column = 40 } }
  { typ = (STRING "\n");
    start_pos = { filename = "./data.fe"; line = 14; column = 40 };
    end_pos = { filename = "./data.fe"; line = 14; column = 44 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 14; column = 44 };
    end_pos = { filename = "./data.fe"; line = 14; column = 45 } }
  { typ = (STRING "true || false = ");
    start_pos = { filename = "./data.fe"; line = 15; column = 7 };
    end_pos = { filename = "./data.fe"; line = 15; column = 23 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 15; column = 23 };
    end_pos = { filename = "./data.fe"; line = 15; column = 24 } }
  { typ = KEYWORD_TRUE;
    start_pos = { filename = "./data.fe"; line = 15; column = 24 };
    end_pos = { filename = "./data.fe"; line = 15; column = 28 } }
  { typ = LOGICAL_OR;
    start_pos = { filename = "./data.fe"; line = 15; column = 29 };
    end_pos = { filename = "./data.fe"; line = 15; column = 31 } }
  { typ = KEYWORD_FALSE;
    start_pos = { filename = "./data.fe"; line = 15; column = 32 };
    end_pos = { filename = "./data.fe"; line = 15; column = 37 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 15; column = 37 };
    end_pos = { filename = "./data.fe"; line = 15; column = 38 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 15; column = 39 };
    end_pos = { filename = "./data.fe"; line = 15; column = 40 } }
  { typ = (STRING "\n");
    start_pos = { filename = "./data.fe"; line = 15; column = 40 };
    end_pos = { filename = "./data.fe"; line = 15; column = 44 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 15; column = 44 };
    end_pos = { filename = "./data.fe"; line = 15; column = 45 } }
  { typ = (STRING "false || false = ");
    start_pos = { filename = "./data.fe"; line = 16; column = 7 };
    end_pos = { filename = "./data.fe"; line = 16; column = 24 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 16; column = 24 };
    end_pos = { filename = "./data.fe"; line = 16; column = 25 } }
  { typ = KEYWORD_FALSE;
    start_pos = { filename = "./data.fe"; line = 16; column = 25 };
    end_pos = { filename = "./data.fe"; line = 16; column = 30 } }
  { typ = LOGICAL_OR;
    start_pos = { filename = "./data.fe"; line = 16; column = 31 };
    end_pos = { filename = "./data.fe"; line = 16; column = 33 } }
  { typ = KEYWORD_FALSE;
    start_pos = { filename = "./data.fe"; line = 16; column = 34 };
    end_pos = { filename = "./data.fe"; line = 16; column = 39 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 16; column = 39 };
    end_pos = { filename = "./data.fe"; line = 16; column = 40 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 16; column = 41 };
    end_pos = { filename = "./data.fe"; line = 16; column = 42 } }
  { typ = (STRING "\n");
    start_pos = { filename = "./data.fe"; line = 16; column = 42 };
    end_pos = { filename = "./data.fe"; line = 16; column = 46 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 16; column = 46 };
    end_pos = { filename = "./data.fe"; line = 16; column = 47 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 17; column = 7 };
    end_pos = { filename = "./data.fe"; line = 17; column = 8 } }
  { typ = (STRING "\n");
    start_pos = { filename = "./data.fe"; line = 17; column = 8 };
    end_pos = { filename = "./data.fe"; line = 17; column = 12 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 17; column = 12 };
    end_pos = { filename = "./data.fe"; line = 17; column = 13 } }
  { typ = (STRING "1 > 2 = ");
    start_pos = { filename = "./data.fe"; line = 18; column = 7 };
    end_pos = { filename = "./data.fe"; line = 18; column = 15 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 18; column = 15 };
    end_pos = { filename = "./data.fe"; line = 18; column = 16 } }
  { typ = (INT 1);
    start_pos = { filename = "./data.fe"; line = 18; column = 16 };
    end_pos = { filename = "./data.fe"; line = 18; column = 17 } }
  { typ = GREATER;
    start_pos = { filename = "./data.fe"; line = 18; column = 18 };
    end_pos = { filename = "./data.fe"; line = 18; column = 19 } }
  { typ = (INT 2);
    start_pos = { filename = "./data.fe"; line = 18; column = 20 };
    end_pos = { filename = "./data.fe"; line = 18; column = 21 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 18; column = 21 };
    end_pos = { filename = "./data.fe"; line = 18; column = 22 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 18; column = 23 };
    end_pos = { filename = "./data.fe"; line = 18; column = 24 } }
  { typ = (STRING "\n");
    start_pos = { filename = "./data.fe"; line = 18; column = 24 };
    end_pos = { filename = "./data.fe"; line = 18; column = 28 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 18; column = 28 };
    end_pos = { filename = "./data.fe"; line = 18; column = 29 } }
  { typ = (STRING "1 < 2 = ");
    start_pos = { filename = "./data.fe"; line = 19; column = 7 };
    end_pos = { filename = "./data.fe"; line = 19; column = 15 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 19; column = 15 };
    end_pos = { filename = "./data.fe"; line = 19; column = 16 } }
  { typ = (INT 1);
    start_pos = { filename = "./data.fe"; line = 19; column = 16 };
    end_pos = { filename = "./data.fe"; line = 19; column = 17 } }
  { typ = LESS; start_pos = { filename = "./data.fe"; line = 19; column = 18 };
    end_pos = { filename = "./data.fe"; line = 19; column = 19 } }
  { typ = (INT 2);
    start_pos = { filename = "./data.fe"; line = 19; column = 20 };
    end_pos = { filename = "./data.fe"; line = 19; column = 21 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 19; column = 21 };
    end_pos = { filename = "./data.fe"; line = 19; column = 22 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 19; column = 23 };
    end_pos = { filename = "./data.fe"; line = 19; column = 24 } }
  { typ = (STRING "\n");
    start_pos = { filename = "./data.fe"; line = 19; column = 24 };
    end_pos = { filename = "./data.fe"; line = 19; column = 28 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 19; column = 28 };
    end_pos = { filename = "./data.fe"; line = 19; column = 29 } }
  { typ = (STRING "1 <= 1 = ");
    start_pos = { filename = "./data.fe"; line = 20; column = 7 };
    end_pos = { filename = "./data.fe"; line = 20; column = 16 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 20; column = 16 };
    end_pos = { filename = "./data.fe"; line = 20; column = 17 } }
  { typ = (INT 1);
    start_pos = { filename = "./data.fe"; line = 20; column = 17 };
    end_pos = { filename = "./data.fe"; line = 20; column = 18 } }
  { typ = LESS_EQUAL;
    start_pos = { filename = "./data.fe"; line = 20; column = 19 };
    end_pos = { filename = "./data.fe"; line = 20; column = 21 } }
  { typ = (INT 1);
    start_pos = { filename = "./data.fe"; line = 20; column = 22 };
    end_pos = { filename = "./data.fe"; line = 20; column = 23 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 20; column = 23 };
    end_pos = { filename = "./data.fe"; line = 20; column = 24 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 20; column = 25 };
    end_pos = { filename = "./data.fe"; line = 20; column = 26 } }
  { typ = (STRING "\n");
    start_pos = { filename = "./data.fe"; line = 20; column = 26 };
    end_pos = { filename = "./data.fe"; line = 20; column = 30 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 20; column = 30 };
    end_pos = { filename = "./data.fe"; line = 20; column = 31 } }
  { typ = (STRING "2 <= 1 = ");
    start_pos = { filename = "./data.fe"; line = 21; column = 7 };
    end_pos = { filename = "./data.fe"; line = 21; column = 16 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 21; column = 16 };
    end_pos = { filename = "./data.fe"; line = 21; column = 17 } }
  { typ = (INT 2);
    start_pos = { filename = "./data.fe"; line = 21; column = 17 };
    end_pos = { filename = "./data.fe"; line = 21; column = 18 } }
  { typ = LESS_EQUAL;
    start_pos = { filename = "./data.fe"; line = 21; column = 19 };
    end_pos = { filename = "./data.fe"; line = 21; column = 21 } }
  { typ = (INT 1);
    start_pos = { filename = "./data.fe"; line = 21; column = 22 };
    end_pos = { filename = "./data.fe"; line = 21; column = 23 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 21; column = 23 };
    end_pos = { filename = "./data.fe"; line = 21; column = 24 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 21; column = 25 };
    end_pos = { filename = "./data.fe"; line = 21; column = 26 } }
  { typ = (STRING "\n");
    start_pos = { filename = "./data.fe"; line = 21; column = 26 };
    end_pos = { filename = "./data.fe"; line = 21; column = 30 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 21; column = 30 };
    end_pos = { filename = "./data.fe"; line = 21; column = 31 } }
  { typ = (STRING "1 >= 1 = ");
    start_pos = { filename = "./data.fe"; line = 22; column = 7 };
    end_pos = { filename = "./data.fe"; line = 22; column = 16 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 22; column = 16 };
    end_pos = { filename = "./data.fe"; line = 22; column = 17 } }
  { typ = (INT 1);
    start_pos = { filename = "./data.fe"; line = 22; column = 17 };
    end_pos = { filename = "./data.fe"; line = 22; column = 18 } }
  { typ = GREATER_EQUAL;
    start_pos = { filename = "./data.fe"; line = 22; column = 19 };
    end_pos = { filename = "./data.fe"; line = 22; column = 21 } }
  { typ = (INT 1);
    start_pos = { filename = "./data.fe"; line = 22; column = 22 };
    end_pos = { filename = "./data.fe"; line = 22; column = 23 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 22; column = 23 };
    end_pos = { filename = "./data.fe"; line = 22; column = 24 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 22; column = 25 };
    end_pos = { filename = "./data.fe"; line = 22; column = 26 } }
  { typ = (STRING "\n");
    start_pos = { filename = "./data.fe"; line = 22; column = 26 };
    end_pos = { filename = "./data.fe"; line = 22; column = 30 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 22; column = 30 };
    end_pos = { filename = "./data.fe"; line = 22; column = 31 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 23; column = 7 };
    end_pos = { filename = "./data.fe"; line = 23; column = 8 } }
  { typ = (STRING "\n");
    start_pos = { filename = "./data.fe"; line = 23; column = 8 };
    end_pos = { filename = "./data.fe"; line = 23; column = 12 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 23; column = 12 };
    end_pos = { filename = "./data.fe"; line = 23; column = 13 } }
  { typ = (STRING "[1] == [1] = ");
    start_pos = { filename = "./data.fe"; line = 24; column = 7 };
    end_pos = { filename = "./data.fe"; line = 24; column = 20 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 24; column = 20 };
    end_pos = { filename = "./data.fe"; line = 24; column = 21 } }
  { typ = LEFT_BRACK;
    start_pos = { filename = "./data.fe"; line = 24; column = 21 };
    end_pos = { filename = "./data.fe"; line = 24; column = 22 } }
  { typ = (INT 1);
    start_pos = { filename = "./data.fe"; line = 24; column = 22 };
    end_pos = { filename = "./data.fe"; line = 24; column = 23 } }
  { typ = RIGHT_BRACK;
    start_pos = { filename = "./data.fe"; line = 24; column = 23 };
    end_pos = { filename = "./data.fe"; line = 24; column = 24 } }
  { typ = EQUAL_EQUAL;
    start_pos = { filename = "./data.fe"; line = 24; column = 25 };
    end_pos = { filename = "./data.fe"; line = 24; column = 27 } }
  { typ = LEFT_BRACK;
    start_pos = { filename = "./data.fe"; line = 24; column = 28 };
    end_pos = { filename = "./data.fe"; line = 24; column = 29 } }
  { typ = (INT 1);
    start_pos = { filename = "./data.fe"; line = 24; column = 29 };
    end_pos = { filename = "./data.fe"; line = 24; column = 30 } }
  { typ = RIGHT_BRACK;
    start_pos = { filename = "./data.fe"; line = 24; column = 30 };
    end_pos = { filename = "./data.fe"; line = 24; column = 31 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 24; column = 31 };
    end_pos = { filename = "./data.fe"; line = 24; column = 32 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 24; column = 33 };
    end_pos = { filename = "./data.fe"; line = 24; column = 34 } }
  { typ = (STRING "\n");
    start_pos = { filename = "./data.fe"; line = 24; column = 34 };
    end_pos = { filename = "./data.fe"; line = 24; column = 38 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 24; column = 38 };
    end_pos = { filename = "./data.fe"; line = 24; column = 39 } }
  { typ = (STRING "[1] == [2] = ");
    start_pos = { filename = "./data.fe"; line = 25; column = 7 };
    end_pos = { filename = "./data.fe"; line = 25; column = 20 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 25; column = 20 };
    end_pos = { filename = "./data.fe"; line = 25; column = 21 } }
  { typ = LEFT_BRACK;
    start_pos = { filename = "./data.fe"; line = 25; column = 21 };
    end_pos = { filename = "./data.fe"; line = 25; column = 22 } }
  { typ = (INT 1);
    start_pos = { filename = "./data.fe"; line = 25; column = 22 };
    end_pos = { filename = "./data.fe"; line = 25; column = 23 } }
  { typ = RIGHT_BRACK;
    start_pos = { filename = "./data.fe"; line = 25; column = 23 };
    end_pos = { filename = "./data.fe"; line = 25; column = 24 } }
  { typ = EQUAL_EQUAL;
    start_pos = { filename = "./data.fe"; line = 25; column = 25 };
    end_pos = { filename = "./data.fe"; line = 25; column = 27 } }
  { typ = LEFT_BRACK;
    start_pos = { filename = "./data.fe"; line = 25; column = 28 };
    end_pos = { filename = "./data.fe"; line = 25; column = 29 } }
  { typ = (INT 2);
    start_pos = { filename = "./data.fe"; line = 25; column = 29 };
    end_pos = { filename = "./data.fe"; line = 25; column = 30 } }
  { typ = RIGHT_BRACK;
    start_pos = { filename = "./data.fe"; line = 25; column = 30 };
    end_pos = { filename = "./data.fe"; line = 25; column = 31 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 25; column = 31 };
    end_pos = { filename = "./data.fe"; line = 25; column = 32 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 25; column = 33 };
    end_pos = { filename = "./data.fe"; line = 25; column = 34 } }
  { typ = (STRING "\n");
    start_pos = { filename = "./data.fe"; line = 25; column = 34 };
    end_pos = { filename = "./data.fe"; line = 25; column = 38 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 25; column = 38 };
    end_pos = { filename = "./data.fe"; line = 25; column = 39 } }
  { typ = (STRING "[1, 2] >= [5] = ");
    start_pos = { filename = "./data.fe"; line = 26; column = 7 };
    end_pos = { filename = "./data.fe"; line = 26; column = 23 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 26; column = 23 };
    end_pos = { filename = "./data.fe"; line = 26; column = 24 } }
  { typ = LEFT_BRACK;
    start_pos = { filename = "./data.fe"; line = 26; column = 24 };
    end_pos = { filename = "./data.fe"; line = 26; column = 25 } }
  { typ = (INT 1);
    start_pos = { filename = "./data.fe"; line = 26; column = 25 };
    end_pos = { filename = "./data.fe"; line = 26; column = 26 } }
  { typ = COMMA;
    start_pos = { filename = "./data.fe"; line = 26; column = 26 };
    end_pos = { filename = "./data.fe"; line = 26; column = 27 } }
  { typ = (INT 2);
    start_pos = { filename = "./data.fe"; line = 26; column = 28 };
    end_pos = { filename = "./data.fe"; line = 26; column = 29 } }
  { typ = RIGHT_BRACK;
    start_pos = { filename = "./data.fe"; line = 26; column = 29 };
    end_pos = { filename = "./data.fe"; line = 26; column = 30 } }
  { typ = GREATER_EQUAL;
    start_pos = { filename = "./data.fe"; line = 26; column = 31 };
    end_pos = { filename = "./data.fe"; line = 26; column = 33 } }
  { typ = LEFT_BRACK;
    start_pos = { filename = "./data.fe"; line = 26; column = 34 };
    end_pos = { filename = "./data.fe"; line = 26; column = 35 } }
  { typ = (INT 5);
    start_pos = { filename = "./data.fe"; line = 26; column = 35 };
    end_pos = { filename = "./data.fe"; line = 26; column = 36 } }
  { typ = RIGHT_BRACK;
    start_pos = { filename = "./data.fe"; line = 26; column = 36 };
    end_pos = { filename = "./data.fe"; line = 26; column = 37 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 26; column = 37 };
    end_pos = { filename = "./data.fe"; line = 26; column = 38 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 26; column = 39 };
    end_pos = { filename = "./data.fe"; line = 26; column = 40 } }
  { typ = (STRING "\n");
    start_pos = { filename = "./data.fe"; line = 26; column = 40 };
    end_pos = { filename = "./data.fe"; line = 26; column = 44 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 26; column = 44 };
    end_pos = { filename = "./data.fe"; line = 26; column = 45 } }
  { typ = (STRING "[1] >= [1,2,3] = ");
    start_pos = { filename = "./data.fe"; line = 27; column = 7 };
    end_pos = { filename = "./data.fe"; line = 27; column = 24 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 27; column = 24 };
    end_pos = { filename = "./data.fe"; line = 27; column = 25 } }
  { typ = LEFT_BRACK;
    start_pos = { filename = "./data.fe"; line = 27; column = 25 };
    end_pos = { filename = "./data.fe"; line = 27; column = 26 } }
  { typ = (INT 1);
    start_pos = { filename = "./data.fe"; line = 27; column = 26 };
    end_pos = { filename = "./data.fe"; line = 27; column = 27 } }
  { typ = RIGHT_BRACK;
    start_pos = { filename = "./data.fe"; line = 27; column = 27 };
    end_pos = { filename = "./data.fe"; line = 27; column = 28 } }
  { typ = GREATER_EQUAL;
    start_pos = { filename = "./data.fe"; line = 27; column = 29 };
    end_pos = { filename = "./data.fe"; line = 27; column = 31 } }
  { typ = LEFT_BRACK;
    start_pos = { filename = "./data.fe"; line = 27; column = 32 };
    end_pos = { filename = "./data.fe"; line = 27; column = 33 } }
  { typ = (INT 1);
    start_pos = { filename = "./data.fe"; line = 27; column = 33 };
    end_pos = { filename = "./data.fe"; line = 27; column = 34 } }
  { typ = COMMA;
    start_pos = { filename = "./data.fe"; line = 27; column = 34 };
    end_pos = { filename = "./data.fe"; line = 27; column = 35 } }
  { typ = (INT 2);
    start_pos = { filename = "./data.fe"; line = 27; column = 35 };
    end_pos = { filename = "./data.fe"; line = 27; column = 36 } }
  { typ = COMMA;
    start_pos = { filename = "./data.fe"; line = 27; column = 36 };
    end_pos = { filename = "./data.fe"; line = 27; column = 37 } }
  { typ = (INT 3);
    start_pos = { filename = "./data.fe"; line = 27; column = 37 };
    end_pos = { filename = "./data.fe"; line = 27; column = 38 } }
  { typ = RIGHT_BRACK;
    start_pos = { filename = "./data.fe"; line = 27; column = 38 };
    end_pos = { filename = "./data.fe"; line = 27; column = 39 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 27; column = 39 };
    end_pos = { filename = "./data.fe"; line = 27; column = 40 } }
  { typ = LEFT_BRACE;
    start_pos = { filename = "./data.fe"; line = 27; column = 41 };
    end_pos = { filename = "./data.fe"; line = 27; column = 42 } }
  { typ = (STRING "\n");
    start_pos = { filename = "./data.fe"; line = 27; column = 42 };
    end_pos = { filename = "./data.fe"; line = 27; column = 46 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 27; column = 46 };
    end_pos = { filename = "./data.fe"; line = 27; column = 47 } }
  { typ = (HTML_CLOSE_TAG "div");
    start_pos = { filename = "./data.fe"; line = 29; column = 5 };
    end_pos = { filename = "./data.fe"; line = 29; column = 11 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 30; column = 3 };
    end_pos = { filename = "./data.fe"; line = 30; column = 4 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 31; column = 1 };
    end_pos = { filename = "./data.fe"; line = 31; column = 2 } }
  { typ = END_OF_INPUT;
    start_pos = { filename = "./data.fe"; line = 32; column = 1 };
    end_pos = { filename = "./data.fe"; line = 32; column = 1 } }


  $ esy x print_ast ./data.fe
  [ComponentDeclaration {
     location = { filename = "./data.fe"; line = 1; column = 1 };
     identifier = (Id "Operators"); attributes = None;
     body =
     [(ExpressionStmt
         (BlockExpression
            [(ExpressionStmt
                (TemplateExpression
                   [HtmlTemplateNode {tag = "div"; attributes = [];
                      children =
                      [(ExpressionTemplateNode
                          (LiteralExpression (StringLiteral "\n")));
                        (TextTemplateNode "![1] = ");
                        (ExpressionTemplateNode
                           UnaryExpression {operator = NOT;
                             argument =
                             (ArrayExpression
                                [(LiteralExpression (IntLiteral 1))])});
                        (ExpressionTemplateNode
                           (LiteralExpression (StringLiteral "\n")));
                        (TextTemplateNode "![] = ");
                        (ExpressionTemplateNode
                           UnaryExpression {operator = NOT;
                             argument = (ArrayExpression [])});
                        (ExpressionTemplateNode
                           (LiteralExpression (StringLiteral "\n")));
                        (TextTemplateNode "!!\"\" = ");
                        (ExpressionTemplateNode
                           UnaryExpression {operator = NOT;
                             argument =
                             UnaryExpression {operator = NOT;
                               argument =
                               (LiteralExpression (StringLiteral ""))}});
                        (ExpressionTemplateNode
                           (LiteralExpression (StringLiteral "\n")));
                        (TextTemplateNode "!true = ");
                        (ExpressionTemplateNode
                           UnaryExpression {operator = NOT;
                             argument = (LiteralExpression (BoolLiteral true))});
                        (ExpressionTemplateNode
                           (LiteralExpression (StringLiteral "\n")));
                        (TextTemplateNode "-1 = ");
                        (ExpressionTemplateNode
                           UnaryExpression {operator = NEGATIVE;
                             argument = (LiteralExpression (IntLiteral 1))});
                        (ExpressionTemplateNode
                           (LiteralExpression (StringLiteral "\n")));
                        (TextTemplateNode "-2.5 = ");
                        (ExpressionTemplateNode
                           UnaryExpression {operator = NEGATIVE;
                             argument = (LiteralExpression (FloatLiteral 2.5))});
                        (ExpressionTemplateNode
                           (LiteralExpression (StringLiteral "\n")));
                        (ExpressionTemplateNode
                           (LiteralExpression (StringLiteral "\n")));
                        (TextTemplateNode "true == true = ");
                        (ExpressionTemplateNode
                           BinaryExpression {
                             left = (LiteralExpression (BoolLiteral true));
                             operator = EQUAL;
                             right = (LiteralExpression (BoolLiteral true))});
                        (ExpressionTemplateNode
                           (LiteralExpression (StringLiteral "\n")));
                        (TextTemplateNode "true != true = ");
                        (ExpressionTemplateNode
                           BinaryExpression {
                             left = (LiteralExpression (BoolLiteral true));
                             operator = NOT_EQUAL;
                             right = (LiteralExpression (BoolLiteral true))});
                        (ExpressionTemplateNode
                           (LiteralExpression (StringLiteral "\n")));
                        (TextTemplateNode "true && true = ");
                        (ExpressionTemplateNode
                           BinaryExpression {
                             left = (LiteralExpression (BoolLiteral true));
                             operator = AND;
                             right = (LiteralExpression (BoolLiteral true))});
                        (ExpressionTemplateNode
                           (LiteralExpression (StringLiteral "\n")));
                        (TextTemplateNode "true && false = ");
                        (ExpressionTemplateNode
                           BinaryExpression {
                             left = (LiteralExpression (BoolLiteral true));
                             operator = AND;
                             right = (LiteralExpression (BoolLiteral false))});
                        (ExpressionTemplateNode
                           (LiteralExpression (StringLiteral "\n")));
                        (TextTemplateNode "true || false = ");
                        (ExpressionTemplateNode
                           BinaryExpression {
                             left = (LiteralExpression (BoolLiteral true));
                             operator = OR;
                             right = (LiteralExpression (BoolLiteral false))});
                        (ExpressionTemplateNode
                           (LiteralExpression (StringLiteral "\n")));
                        (TextTemplateNode "false || false = ");
                        (ExpressionTemplateNode
                           BinaryExpression {
                             left = (LiteralExpression (BoolLiteral false));
                             operator = OR;
                             right = (LiteralExpression (BoolLiteral false))});
                        (ExpressionTemplateNode
                           (LiteralExpression (StringLiteral "\n")));
                        (ExpressionTemplateNode
                           (LiteralExpression (StringLiteral "\n")));
                        (TextTemplateNode "1 > 2 = ");
                        (ExpressionTemplateNode
                           BinaryExpression {
                             left = (LiteralExpression (IntLiteral 1));
                             operator = GREATER;
                             right = (LiteralExpression (IntLiteral 2))});
                        (ExpressionTemplateNode
                           (LiteralExpression (StringLiteral "\n")));
                        (TextTemplateNode "1 < 2 = ");
                        (ExpressionTemplateNode
                           BinaryExpression {
                             left = (LiteralExpression (IntLiteral 1));
                             operator = LESS;
                             right = (LiteralExpression (IntLiteral 2))});
                        (ExpressionTemplateNode
                           (LiteralExpression (StringLiteral "\n")));
                        (TextTemplateNode "1 <= 1 = ");
                        (ExpressionTemplateNode
                           BinaryExpression {
                             left = (LiteralExpression (IntLiteral 1));
                             operator = LESS_EQUAL;
                             right = (LiteralExpression (IntLiteral 1))});
                        (ExpressionTemplateNode
                           (LiteralExpression (StringLiteral "\n")));
                        (TextTemplateNode "2 <= 1 = ");
                        (ExpressionTemplateNode
                           BinaryExpression {
                             left = (LiteralExpression (IntLiteral 2));
                             operator = LESS_EQUAL;
                             right = (LiteralExpression (IntLiteral 1))});
                        (ExpressionTemplateNode
                           (LiteralExpression (StringLiteral "\n")));
                        (TextTemplateNode "1 >= 1 = ");
                        (ExpressionTemplateNode
                           BinaryExpression {
                             left = (LiteralExpression (IntLiteral 1));
                             operator = GREATER_EQUAL;
                             right = (LiteralExpression (IntLiteral 1))});
                        (ExpressionTemplateNode
                           (LiteralExpression (StringLiteral "\n")));
                        (ExpressionTemplateNode
                           (LiteralExpression (StringLiteral "\n")));
                        (TextTemplateNode "[1] == [1] = ");
                        (ExpressionTemplateNode
                           BinaryExpression {
                             left =
                             (ArrayExpression
                                [(LiteralExpression (IntLiteral 1))]);
                             operator = EQUAL;
                             right =
                             (ArrayExpression
                                [(LiteralExpression (IntLiteral 1))])});
                        (ExpressionTemplateNode
                           (LiteralExpression (StringLiteral "\n")));
                        (TextTemplateNode "[1] == [2] = ");
                        (ExpressionTemplateNode
                           BinaryExpression {
                             left =
                             (ArrayExpression
                                [(LiteralExpression (IntLiteral 1))]);
                             operator = EQUAL;
                             right =
                             (ArrayExpression
                                [(LiteralExpression (IntLiteral 2))])});
                        (ExpressionTemplateNode
                           (LiteralExpression (StringLiteral "\n")));
                        (TextTemplateNode "[1, 2] >= [5] = ");
                        (ExpressionTemplateNode
                           BinaryExpression {
                             left =
                             (ArrayExpression
                                [(LiteralExpression (IntLiteral 1));
                                  (LiteralExpression (IntLiteral 2))]);
                             operator = GREATER_EQUAL;
                             right =
                             (ArrayExpression
                                [(LiteralExpression (IntLiteral 5))])});
                        (ExpressionTemplateNode
                           (LiteralExpression (StringLiteral "\n")));
                        (TextTemplateNode "[1] >= [1,2,3] = ");
                        (ExpressionTemplateNode
                           BinaryExpression {
                             left =
                             (ArrayExpression
                                [(LiteralExpression (IntLiteral 1))]);
                             operator = GREATER_EQUAL;
                             right =
                             (ArrayExpression
                                [(LiteralExpression (IntLiteral 1));
                                  (LiteralExpression (IntLiteral 2));
                                  (LiteralExpression (IntLiteral 3))])});
                        (ExpressionTemplateNode
                           (LiteralExpression (StringLiteral "\n")))
                        ]}
                     ]))
              ]))
       ]}
    ]
