  $ esy x print ./data.fe Section
  <div class="SubComponent" data-foo="bar">Hello, SubComponent!</div><section class="Section"></section>


  $ esy x print_ast ./data.fe
  [ComponentDeclaration {
     location = { filename = "./data.fe"; line = 1; column = 1 };
     identifier = (Id "SubComponent"); attributes = None;
     body =
     [(ExpressionStmt
         (BlockExpression
            [DeclarationStmt {nullable = false; left = (Id "content");
               right =
               (LiteralExpression (StringLiteral "Hello, SubComponent!"))};
              (ExpressionStmt
                 (TemplateExpression
                    [HtmlTemplateNode {tag = "div";
                       attributes =
                       [{ key = "class";
                          value =
                          (LiteralExpression (StringLiteral "SubComponent")) };
                         { key = "data-foo";
                           value = (LiteralExpression (StringLiteral "bar")) }
                         ];
                       children =
                       [(ExpressionTemplateNode
                           (IdentifierExpression (Id "content")))
                         ]}
                      ]))
              ]))
       ]};
    ComponentDeclaration {
      location = { filename = "./data.fe"; line = 11; column = 1 };
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
             [(ExpressionStmt
                 (TemplateExpression
                    [HtmlTemplateNode {tag = "section";
                       attributes =
                       [{ key = "class";
                          value = (LiteralExpression (StringLiteral "Section"))
                          }
                         ];
                       children =
                       [ComponentTemplateNode {
                          identifier = (Id "SubComponent"); attributes = [];
                          children = []}
                         ]}
                      ]))
               ]))
        ]}
    ]
