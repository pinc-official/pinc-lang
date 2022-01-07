  $ esy x print_ast ./data.fe
  [ComponentDeclaration {
     location = { filename = "./data.fe"; line = 1; column = 1 };
     identifier = (Id "Section");
     attributes =
     (Some [{ key = "label";
              value = (LiteralExpression (StringLiteral "Section")) };
             { key = "icon";
               value =
               (LiteralExpression (StringLiteral "/images/icons/group.svg")) };
             { key = "group";
               value = (LiteralExpression (StringLiteral "Structure")) }
             ]);
     body =
     [(ExpressionStmt
         (BlockExpression
            [DeclarationStmt {nullable = false; left = (Id "content");
               right = (LiteralExpression (StringLiteral "Hello, World!"))};
              (ExpressionStmt
                 (TemplateExpression
                    [HtmlTemplateNode {tag = "section"; self_closing = false;
                       attributes =
                       [{ key = "class";
                          value = (LiteralExpression (StringLiteral "Section"))
                          }
                         ];
                       children =
                       [(ExpressionTemplateNode
                           ConditionalExpression {
                             condition = (IdentifierExpression (Id "intro"));
                             consequent =
                             (BlockExpression
                                [DeclarationStmt {nullable = false;
                                   left = (Id "foobar");
                                   right =
                                   (LiteralExpression (StringLiteral "foo"))};
                                  (ExpressionStmt
                                     ConditionalExpression {
                                       condition =
                                       (IdentifierExpression (Id "foobar"));
                                       consequent =
                                       (BlockExpression
                                          [(ExpressionStmt
                                              (TemplateExpression
                                                 [HtmlTemplateNode {
                                                    tag = "div";
                                                    self_closing = true;
                                                    attributes = [];
                                                    children = []}
                                                   ]))
                                            ]);
                                       alternate =
                                       (Some (BlockExpression
                                                [DeclarationStmt {
                                                   nullable = false;
                                                   left = (Id "barfoo");
                                                   right =
                                                   (LiteralExpression
                                                      (StringLiteral "3"))};
                                                  (ExpressionStmt
                                                     (TemplateExpression
                                                        [HtmlTemplateNode {
                                                           tag = "span";
                                                           self_closing = true;
                                                           attributes = [];
                                                           children = []}
                                                          ]))
                                                  ]))})
                                  ]);
                             alternate = None});
                         HtmlTemplateNode {tag = "div"; self_closing = false;
                           attributes =
                           [{ key = "class";
                              value =
                              (LiteralExpression
                                 (StringLiteral "Section-content"))
                              }
                             ];
                           children =
                           [(ExpressionTemplateNode
                               (IdentifierExpression (Id "content")))
                             ]}
                         ]}
                      ]))
              ]))
       ]}
    ]
