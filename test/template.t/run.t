  $ esy x print ./data.fe
  <section class="Section"><div></div><div></div><div class="Section-content">Hello, World!</div><div class="Section-footer">Lorem Ipsum dolor sit amet!!
        </div></section>


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
              DeclarationStmt {nullable = false; left = (Id "intro");
                right = (LiteralExpression (BoolLiteral true))};
              (ExpressionStmt
                 (TemplateExpression
                    [HtmlTemplateNode {tag = "section"; self_closing = false;
                       attributes =
                       [{ key = "class";
                          value = (LiteralExpression (StringLiteral "Section"))
                          }
                         ];
                       children =
                       [HtmlTemplateNode {tag = "div"; self_closing = true;
                          attributes = []; children = []};
                         (ExpressionTemplateNode
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
                             ]};
                         HtmlTemplateNode {tag = "div"; self_closing = false;
                           attributes =
                           [{ key = "class";
                              value =
                              (LiteralExpression
                                 (StringLiteral "Section-footer"))
                              }
                             ];
                           children =
                           [(TextTemplateNode
                               "Lorem Ipsum dolor sit amet!!\n      ")
                             ]}
                         ]}
                      ]))
              ]))
       ]}
    ]
