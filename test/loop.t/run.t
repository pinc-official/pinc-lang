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
  <div class="item">10</div>
  
  0 to 0
        <div class="item">0</div>
  
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
  <div class="item">10</div>
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
                                ]})
                         ]}
                      ]))
              ]))
       ]}
    ]
