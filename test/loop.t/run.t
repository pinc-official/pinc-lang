  $ esy x print ./data.fe Section
  <section>0 to 10
        <div class="item">0</div>
  <div class="item">1</div>
  <div class="item">2</div>
  <div class="item">3</div>
  <div class="item">4</div>
  <div class="item">5</div>
  <div class="item">6</div>
  <div class="item">7</div>
  <div class="item">8</div>
  <div class="item">9</div>
  
  0 to 0
        
  -10 to 10
        <div class="item">-10</div>
  <div class="item">-9</div>
  <div class="item">-8</div>
  <div class="item">-7</div>
  <div class="item">-6</div>
  <div class="item">-5</div>
  <div class="item">-4</div>
  <div class="item">-3</div>
  <div class="item">-2</div>
  <div class="item">-1</div>
  <div class="item">0</div>
  <div class="item">1</div>
  <div class="item">2</div>
  <div class="item">3</div>
  <div class="item">4</div>
  <div class="item">5</div>
  <div class="item">6</div>
  <div class="item">7</div>
  <div class="item">8</div>
  <div class="item">9</div>
  
  array
        <div class="item">0</div>
  <div class="item">1</div>
  <div class="item">2</div>
  <div class="item">3</div>
  <div class="item">4</div>
  <div class="item">5</div>
  <div class="item">6</div>
  <div class="item">7</div>
  <div class="item">8</div>
  <div class="item">9</div>
  
  string
        <div class="item">S</div>
  <div class="item">t</div>
  <div class="item">r</div>
  <div class="item">i</div>
  <div class="item">n</div>
  <div class="item">g</div>
  <div class="item">!</div>
  
  string array
        <div class="item">one</div>
  <div class="item">two</div>
  <div class="item">three</div>
  <div class="item">!</div>
  
  null value
        </section>


  $ esy x print_ast ./data.fe
  [ComponentDeclaration {
     location = { filename = "./data.fe"; line = 1; column = 1 };
     identifier = (Id "C"); attributes = None;
     body =
     [(ExpressionStmt
         (BlockExpression
            [DeclarationStmt {nullable = false; left = (Id "max");
               right = (LiteralExpression (IntLiteral 10))};
              DeclarationStmt {nullable = false; left = (Id "array");
                right =
                ForInRangeExpression {iterator = (Id "i");
                  from = (LiteralExpression (IntLiteral 0));
                  upto = (IdentifierExpression (Id "max"));
                  body =
                  [(ExpressionStmt
                      (BlockExpression
                         [(ExpressionStmt (IdentifierExpression (Id "i")))]))
                    ]}};
              DeclarationStmt {nullable = false; left = (Id "string_array");
                right =
                (ArrayExpression
                   [(LiteralExpression (StringLiteral "one"));
                     (LiteralExpression (StringLiteral "two"));
                     (LiteralExpression (StringLiteral "three"));
                     (LiteralExpression (StringLiteral "!"))])};
              DeclarationStmt {nullable = true; left = (Id "null_value");
                right =
                ConditionalExpression {
                  condition = (LiteralExpression (BoolLiteral false));
                  consequent = (LiteralExpression (StringLiteral "something"));
                  alternate = None}};
              (ExpressionStmt
                 (TemplateExpression
                    [HtmlTemplateNode {tag = "section"; attributes = [];
                       children =
                       [(TextTemplateNode "0 to 10\n      ");
                         (ExpressionTemplateNode
                            ForInRangeExpression {iterator = (Id "i");
                              from = (LiteralExpression (IntLiteral 0));
                              upto = (LiteralExpression (IntLiteral 10));
                              body =
                              [(ExpressionStmt
                                  (BlockExpression
                                     [(ExpressionStmt
                                         (TemplateExpression
                                            [HtmlTemplateNode {tag = "div";
                                               attributes =
                                               [{ key = "class";
                                                  value =
                                                  (LiteralExpression
                                                     (StringLiteral "item"))
                                                  }
                                                 ];
                                               children =
                                               [(ExpressionTemplateNode
                                                   (IdentifierExpression
                                                      (Id "i")))
                                                 ]};
                                              (ExpressionTemplateNode
                                                 (LiteralExpression
                                                    (StringLiteral "\n")))
                                              ]))
                                       ]))
                                ]});
                         (ExpressionTemplateNode
                            (LiteralExpression (StringLiteral "\n")));
                         (TextTemplateNode "0 to 0\n      ");
                         (ExpressionTemplateNode
                            ForInRangeExpression {iterator = (Id "i");
                              from = (LiteralExpression (IntLiteral 0));
                              upto = (LiteralExpression (IntLiteral 0));
                              body =
                              [(ExpressionStmt
                                  (BlockExpression
                                     [(ExpressionStmt
                                         (TemplateExpression
                                            [HtmlTemplateNode {tag = "div";
                                               attributes =
                                               [{ key = "class";
                                                  value =
                                                  (LiteralExpression
                                                     (StringLiteral "item"))
                                                  }
                                                 ];
                                               children =
                                               [(ExpressionTemplateNode
                                                   (IdentifierExpression
                                                      (Id "i")))
                                                 ]};
                                              (ExpressionTemplateNode
                                                 (LiteralExpression
                                                    (StringLiteral "\n")))
                                              ]))
                                       ]))
                                ]});
                         (ExpressionTemplateNode
                            (LiteralExpression (StringLiteral "\n")));
                         (TextTemplateNode "-10 to 10\n      ");
                         (ExpressionTemplateNode
                            ForInRangeExpression {iterator = (Id "i");
                              from =
                              UnaryExpression {operator = NEGATIVE;
                                argument = (LiteralExpression (IntLiteral 10))};
                              upto = (LiteralExpression (IntLiteral 10));
                              body =
                              [(ExpressionStmt
                                  (BlockExpression
                                     [(ExpressionStmt
                                         (TemplateExpression
                                            [HtmlTemplateNode {tag = "div";
                                               attributes =
                                               [{ key = "class";
                                                  value =
                                                  (LiteralExpression
                                                     (StringLiteral "item"))
                                                  }
                                                 ];
                                               children =
                                               [(ExpressionTemplateNode
                                                   (IdentifierExpression
                                                      (Id "i")))
                                                 ]};
                                              (ExpressionTemplateNode
                                                 (LiteralExpression
                                                    (StringLiteral "\n")))
                                              ]))
                                       ]))
                                ]});
                         (ExpressionTemplateNode
                            (LiteralExpression (StringLiteral "\n")));
                         (TextTemplateNode "array\n      ");
                         (ExpressionTemplateNode
                            ForInExpression {iterator = (Id "i");
                              iterable = (IdentifierExpression (Id "array"));
                              body =
                              [(ExpressionStmt
                                  (BlockExpression
                                     [(ExpressionStmt
                                         (TemplateExpression
                                            [HtmlTemplateNode {tag = "div";
                                               attributes =
                                               [{ key = "class";
                                                  value =
                                                  (LiteralExpression
                                                     (StringLiteral "item"))
                                                  }
                                                 ];
                                               children =
                                               [(ExpressionTemplateNode
                                                   (IdentifierExpression
                                                      (Id "i")))
                                                 ]};
                                              (ExpressionTemplateNode
                                                 (LiteralExpression
                                                    (StringLiteral "\n")))
                                              ]))
                                       ]))
                                ]});
                         (ExpressionTemplateNode
                            (LiteralExpression (StringLiteral "\n")));
                         (TextTemplateNode "string\n      ");
                         (ExpressionTemplateNode
                            ForInExpression {iterator = (Id "c");
                              iterable =
                              (LiteralExpression (StringLiteral "String!"));
                              body =
                              [(ExpressionStmt
                                  (BlockExpression
                                     [(ExpressionStmt
                                         (TemplateExpression
                                            [HtmlTemplateNode {tag = "div";
                                               attributes =
                                               [{ key = "class";
                                                  value =
                                                  (LiteralExpression
                                                     (StringLiteral "item"))
                                                  }
                                                 ];
                                               children =
                                               [(ExpressionTemplateNode
                                                   (IdentifierExpression
                                                      (Id "c")))
                                                 ]};
                                              (ExpressionTemplateNode
                                                 (LiteralExpression
                                                    (StringLiteral "\n")))
                                              ]))
                                       ]))
                                ]});
                         (ExpressionTemplateNode
                            (LiteralExpression (StringLiteral "\n")));
                         (TextTemplateNode "string array\n      ");
                         (ExpressionTemplateNode
                            ForInExpression {iterator = (Id "s");
                              iterable =
                              (IdentifierExpression (Id "string_array"));
                              body =
                              [(ExpressionStmt
                                  (BlockExpression
                                     [(ExpressionStmt
                                         (TemplateExpression
                                            [HtmlTemplateNode {tag = "div";
                                               attributes =
                                               [{ key = "class";
                                                  value =
                                                  (LiteralExpression
                                                     (StringLiteral "item"))
                                                  }
                                                 ];
                                               children =
                                               [(ExpressionTemplateNode
                                                   (IdentifierExpression
                                                      (Id "s")))
                                                 ]};
                                              (ExpressionTemplateNode
                                                 (LiteralExpression
                                                    (StringLiteral "\n")))
                                              ]))
                                       ]))
                                ]});
                         (ExpressionTemplateNode
                            (LiteralExpression (StringLiteral "\n")));
                         (TextTemplateNode "null value\n      ");
                         (ExpressionTemplateNode
                            ForInExpression {iterator = (Id "s");
                              iterable =
                              (IdentifierExpression (Id "null_value"));
                              body =
                              [(ExpressionStmt
                                  (BlockExpression
                                     [(ExpressionStmt
                                         (TemplateExpression
                                            [HtmlTemplateNode {tag = "div";
                                               attributes =
                                               [{ key = "class";
                                                  value =
                                                  (LiteralExpression
                                                     (StringLiteral "item"))
                                                  }
                                                 ];
                                               children =
                                               [(ExpressionTemplateNode
                                                   (IdentifierExpression
                                                      (Id "s")))
                                                 ]};
                                              (ExpressionTemplateNode
                                                 (LiteralExpression
                                                    (StringLiteral "\n")))
                                              ]))
                                       ]))
                                ]})
                         ]}
                      ]))
              ]))
       ]}
    ]
