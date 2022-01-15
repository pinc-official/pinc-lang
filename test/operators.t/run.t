  $ esy x print ./data.fe Section
  <div>
  ![1] = false
  ![] = true
  !!"" = false
  !true = false
  -1 = -1
  -2.5 = -2.5
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
  { typ = (HTML_CLOSE_TAG "div");
    start_pos = { filename = "./data.fe"; line = 10; column = 5 };
    end_pos = { filename = "./data.fe"; line = 10; column = 11 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 11; column = 3 };
    end_pos = { filename = "./data.fe"; line = 11; column = 4 } }
  { typ = RIGHT_BRACE;
    start_pos = { filename = "./data.fe"; line = 12; column = 1 };
    end_pos = { filename = "./data.fe"; line = 12; column = 2 } }
  { typ = END_OF_INPUT;
    start_pos = { filename = "./data.fe"; line = 13; column = 1 };
    end_pos = { filename = "./data.fe"; line = 13; column = 1 } }


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
                           (LiteralExpression (StringLiteral "\n")))
                        ]}
                     ]))
              ]))
       ]}
    ]
