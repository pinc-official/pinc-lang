  $ esy x print_ast ./data.fe
  [SiteDeclaration {
     location = { filename = "./data.fe"; line = 1; column = 1 };
     identifier = (Id "Docs");
     attributes =
     (Some [{ key = "label";
              value = (LiteralExpression (StringLiteral "Docs Site")) };
             { key = "icon";
               value =
               (LiteralExpression (StringLiteral "/images/icons/site-docs.svg"))
               };
             { key = "domain";
               value =
               (LiteralExpression (StringLiteral "docs.fennek-cms.com")) };
             { key = "additionalDomains";
               value =
               (ArrayExpression
                  [(LiteralExpression (StringLiteral "docs.fennek-cms.de"));
                    (LiteralExpression (StringLiteral "docs.fennek-cms.fr"))])
               }
             ]);
     body =
     [(ExpressionStmt
         (BlockExpression
            [DeclarationStmt {nullable = true; left = (Id "floats");
               right =
               (ArrayExpression
                  [(LiteralExpression (FloatLiteral 1000.462));
                    (LiteralExpression (FloatLiteral 1.));
                    (LiteralExpression (FloatLiteral 1.4332));
                    (LiteralExpression (FloatLiteral 0.5));
                    (LiteralExpression (FloatLiteral 10000000000.));
                    (LiteralExpression (FloatLiteral 100.));
                    (LiteralExpression (FloatLiteral 1e-05));
                    (LiteralExpression (FloatLiteral 0.01));
                    (LiteralExpression (FloatLiteral 0.01))])}
              ]))
       ]}
    ]
