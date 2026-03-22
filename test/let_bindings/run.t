  $ NO_COLOR="1" print . Test
  string
  false

  $ NO_COLOR="1" print_typed . Test
  Test: { Typed_tree.declaration_loc = <opaque>; declaration_type = component;
          declaration_kind =
          (Typed_tree.T_Declaration_Component
             { Typed_tree.declaration_attributes = ;
               declaration_body =
               { Typed_tree.expression_loc = <opaque>; expression_type = t21;
                 expression_desc =
                 (Typed_tree.T_BlockExpression
                    [{ Typed_tree.statement_loc = <opaque>;
                       statement_type = t6;
                       statement_desc =
                       (Typed_tree.T_LetStatement (
                          (Typed_tree.T_Lowercase_Id ("s", string, <opaque>)),
                          { Typed_tree.expression_loc = <opaque>;
                            expression_type = string;
                            expression_desc =
                            (Typed_tree.T_String
                               [{ Typed_tree.string_template_loc = <opaque>;
                                  string_template_type = t5;
                                  string_template_desc =
                                  (Typed_tree.T_StringText "string") }
                                 ])
                            }
                          ))
                       };
                      { Typed_tree.statement_loc = <opaque>;
                        statement_type = t7;
                        statement_desc =
                        (Typed_tree.T_LetStatement (
                           (Typed_tree.T_Lowercase_Id ("i", int, <opaque>)),
                           { Typed_tree.expression_loc = <opaque>;
                             expression_type = int;
                             expression_desc = (Typed_tree.T_Int 3) }
                           ))
                        };
                      { Typed_tree.statement_loc = <opaque>;
                        statement_type = t11;
                        statement_desc =
                        (Typed_tree.T_LetStatement (
                           (Typed_tree.T_Lowercase_Id
                              ("id", fun (t8) -> t8, <opaque>)),
                           { Typed_tree.expression_loc = <opaque>;
                             expression_type = fun (t8) -> t8;
                             expression_desc =
                             Typed_tree.T_Function {
                               identifier =
                               (Some (Typed_tree.T_Lowercase_Id
                                        ("id", t10, <opaque>)));
                               parameters =
                               [(Typed_tree.T_Lowercase_Id ("x", t8, <opaque>))
                                 ];
                               body =
                               { Typed_tree.expression_loc = <opaque>;
                                 expression_type = t8;
                                 expression_desc =
                                 (Typed_tree.T_LowercaseIdentifierExpression
                                    (Typed_tree.T_Lowercase_Id
                                       ("x", t8, <opaque>)))
                                 }}
                             }
                           ))
                        };
                      { Typed_tree.statement_loc = <opaque>;
                        statement_type = t15;
                        statement_desc =
                        (Typed_tree.T_LetStatement (
                           (Typed_tree.T_Lowercase_Id ("foo", t14, <opaque>)),
                           { Typed_tree.expression_loc = <opaque>;
                             expression_type = t14;
                             expression_desc =
                             Typed_tree.T_FunctionCall {
                               function_definition =
                               { Typed_tree.expression_loc = <opaque>;
                                 expression_type = fun (t12) -> t12;
                                 expression_desc =
                                 (Typed_tree.T_LowercaseIdentifierExpression
                                    (Typed_tree.T_Lowercase_Id
                                       ("id", fun (t12) -> t12, <opaque>)))
                                 };
                               arguments =
                               [{ Typed_tree.expression_loc = <opaque>;
                                  expression_type = string;
                                  expression_desc =
                                  (Typed_tree.T_String
                                     [{ Typed_tree.string_template_loc =
                                        <opaque>; string_template_type = t13;
                                        string_template_desc =
                                        (Typed_tree.T_StringText "s") }
                                       ])
                                  }
                                 ]}
                             }
                           ))
                        };
                      { Typed_tree.statement_loc = <opaque>;
                        statement_type = t20;
                        statement_desc =
                        (Typed_tree.T_ExpressionStatement
                           { Typed_tree.expression_loc = <opaque>;
                             expression_type = t19;
                             expression_desc =
                             (Typed_tree.T_TemplateExpression
                                [{ Typed_tree.template_node_loc = <opaque>;
                                   template_node_type = t16;
                                   template_node_desc =
                                   (Typed_tree.T_ExpressionTemplateNode
                                      { Typed_tree.expression_loc = <opaque>;
                                        expression_type = string;
                                        expression_desc =
                                        (Typed_tree.T_LowercaseIdentifierExpression
                                           (Typed_tree.T_Lowercase_Id
                                              ("s", string, <opaque>)))
                                        })
                                   };
                                  { Typed_tree.template_node_loc = <opaque>;
                                    template_node_type = t18;
                                    template_node_desc =
                                    (Typed_tree.T_ExpressionTemplateNode
                                       { Typed_tree.expression_loc = <opaque>;
                                         expression_type = bool;
                                         expression_desc =
                                         (Typed_tree.T_BinaryExpression (
                                            { Typed_tree.expression_loc =
                                              <opaque>; expression_type = int;
                                              expression_desc =
                                              (Typed_tree.T_LowercaseIdentifierExpression
                                                 (Typed_tree.T_Lowercase_Id
                                                    ("i", int, <opaque>)))
                                              },
                                            >,
                                            { Typed_tree.expression_loc =
                                              <opaque>; expression_type = t17;
                                              expression_desc =
                                              (Typed_tree.T_LowercaseIdentifierExpression
                                                 (Typed_tree.T_Lowercase_Id
                                                    ("foo", t17, <opaque>)))
                                              }
                                            ))
                                         })
                                    }
                                  ])
                             })
                        }
                      ])
                 }
               })
          }
 })) }))
 })) }))
 })) }))
 })) }))
