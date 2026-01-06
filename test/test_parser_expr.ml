open Test_helpers.Helpers

(* Literals *)

let%expect_test "integer literal" =
  print_endline (show_parsed_expr "42");
  [%expect
    {|
    { Location.value =
      (Syntax_tree.ExpressionConstant (Syntax_tree.ConstantInteger 42));
      location =
      { Location.start_pos =
        { Location.filename = ""; line = 0; column = 0; offset = 0 };
        end_pos = { Location.filename = ""; line = 0; column = 0; offset = 0 } }
      }
    |}]

let%expect_test "float literal" =
  print_endline (show_parsed_expr "3.14");
  [%expect
    {|
    { Location.value =
      (Syntax_tree.ExpressionConstant (Syntax_tree.ConstantFloat 3.14));
      location =
      { Location.start_pos =
        { Location.filename = ""; line = 0; column = 0; offset = 0 };
        end_pos = { Location.filename = ""; line = 0; column = 0; offset = 0 } }
      }
    |}]

let%expect_test "string literal" =
  print_endline (show_parsed_expr {|"hello"|});
  [%expect
    {|
    { Location.value =
      (Syntax_tree.ExpressionConstant (Syntax_tree.ConstantString "hello"));
      location =
      { Location.start_pos =
        { Location.filename = ""; line = 0; column = 0; offset = 0 };
        end_pos = { Location.filename = ""; line = 0; column = 0; offset = 0 } }
      }
    |}]

let%expect_test "boolean literals" =
  print_endline (show_parsed_expr "true");
  [%expect
    {|
    { Location.value =
      (Syntax_tree.ExpressionConstant (Syntax_tree.ConstantBoolean true));
      location =
      { Location.start_pos =
        { Location.filename = ""; line = 0; column = 0; offset = 0 };
        end_pos = { Location.filename = ""; line = 0; column = 0; offset = 0 } }
      }
    |}]

let%expect_test "unit literal" =
  print_endline (show_parsed_expr "()");
  [%expect
    {|
    File "<string>", line 1, characters 2-2:
    Parse error: Syntax error
    |}]

(* Variables *)

let%expect_test "variable" =
  print_endline (show_parsed_expr "foo");
  [%expect
    {|
    { Location.value = (Syntax_tree.ExpressionVariable "foo");
      location =
      { Location.start_pos =
        { Location.filename = ""; line = 0; column = 0; offset = 0 };
        end_pos = { Location.filename = ""; line = 0; column = 0; offset = 0 } }
      }
    |}]

(* Constructors *)

let%expect_test "constructor without argument" =
  print_endline (show_parsed_expr "None");
  [%expect
    {|
    { Location.value = (Syntax_tree.ExpressionConstructor ("None", None));
      location =
      { Location.start_pos =
        { Location.filename = ""; line = 0; column = 0; offset = 0 };
        end_pos = { Location.filename = ""; line = 0; column = 0; offset = 0 } }
      }
    |}]

let%expect_test "constructor with argument" =
  print_endline (show_parsed_expr "Some 42");
  [%expect
    {|
    { Location.value =
      (Syntax_tree.ExpressionConstructor ("Some",
         (Some { Location.value =
                 (Syntax_tree.ExpressionConstant (Syntax_tree.ConstantInteger 42));
                 location =
                 { Location.start_pos =
                   { Location.filename = ""; line = 0; column = 0; offset = 0 };
                   end_pos =
                   { Location.filename = ""; line = 0; column = 0; offset = 0 } }
                 })
         ));
      location =
      { Location.start_pos =
        { Location.filename = ""; line = 0; column = 0; offset = 0 };
        end_pos = { Location.filename = ""; line = 0; column = 0; offset = 0 } }
      }
    |}]

(* Tuples *)

let%expect_test "pair tuple" =
  print_endline (show_parsed_expr "(1, 2)");
  [%expect
    {|
    { Location.value =
      (Syntax_tree.ExpressionTuple
         [{ Location.value =
            (Syntax_tree.ExpressionConstant (Syntax_tree.ConstantInteger 1));
            location =
            { Location.start_pos =
              { Location.filename = ""; line = 0; column = 0; offset = 0 };
              end_pos =
              { Location.filename = ""; line = 0; column = 0; offset = 0 } }
            };
           { Location.value =
             (Syntax_tree.ExpressionConstant (Syntax_tree.ConstantInteger 2));
             location =
             { Location.start_pos =
               { Location.filename = ""; line = 0; column = 0; offset = 0 };
               end_pos =
               { Location.filename = ""; line = 0; column = 0; offset = 0 } }
             }
           ]);
      location =
      { Location.start_pos =
        { Location.filename = ""; line = 0; column = 0; offset = 0 };
        end_pos = { Location.filename = ""; line = 0; column = 0; offset = 0 } }
      }
    |}]

let%expect_test "triple tuple" =
  print_endline (show_parsed_expr "(1, 2, 3)");
  [%expect
    {|
    { Location.value =
      (Syntax_tree.ExpressionTuple
         [{ Location.value =
            (Syntax_tree.ExpressionConstant (Syntax_tree.ConstantInteger 1));
            location =
            { Location.start_pos =
              { Location.filename = ""; line = 0; column = 0; offset = 0 };
              end_pos =
              { Location.filename = ""; line = 0; column = 0; offset = 0 } }
            };
           { Location.value =
             (Syntax_tree.ExpressionConstant (Syntax_tree.ConstantInteger 2));
             location =
             { Location.start_pos =
               { Location.filename = ""; line = 0; column = 0; offset = 0 };
               end_pos =
               { Location.filename = ""; line = 0; column = 0; offset = 0 } }
             };
           { Location.value =
             (Syntax_tree.ExpressionConstant (Syntax_tree.ConstantInteger 3));
             location =
             { Location.start_pos =
               { Location.filename = ""; line = 0; column = 0; offset = 0 };
               end_pos =
               { Location.filename = ""; line = 0; column = 0; offset = 0 } }
             }
           ]);
      location =
      { Location.start_pos =
        { Location.filename = ""; line = 0; column = 0; offset = 0 };
        end_pos = { Location.filename = ""; line = 0; column = 0; offset = 0 } }
      }
    |}]

(* Function application *)

let%expect_test "simple application" =
  print_endline (show_parsed_expr "f x");
  [%expect
    {|
    { Location.value =
      (Syntax_tree.ExpressionApply (
         { Location.value = (Syntax_tree.ExpressionVariable "f");
           location =
           { Location.start_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 };
             end_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 } }
           },
         [{ Location.value = (Syntax_tree.ExpressionVariable "x");
            location =
            { Location.start_pos =
              { Location.filename = ""; line = 0; column = 0; offset = 0 };
              end_pos =
              { Location.filename = ""; line = 0; column = 0; offset = 0 } }
            }
           ]
         ));
      location =
      { Location.start_pos =
        { Location.filename = ""; line = 0; column = 0; offset = 0 };
        end_pos = { Location.filename = ""; line = 0; column = 0; offset = 0 } }
      }
    |}]

let%expect_test "multi-argument application" =
  print_endline (show_parsed_expr "f x y z");
  [%expect
    {|
    { Location.value =
      (Syntax_tree.ExpressionApply (
         { Location.value = (Syntax_tree.ExpressionVariable "f");
           location =
           { Location.start_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 };
             end_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 } }
           },
         [{ Location.value = (Syntax_tree.ExpressionVariable "x");
            location =
            { Location.start_pos =
              { Location.filename = ""; line = 0; column = 0; offset = 0 };
              end_pos =
              { Location.filename = ""; line = 0; column = 0; offset = 0 } }
            };
           { Location.value = (Syntax_tree.ExpressionVariable "y");
             location =
             { Location.start_pos =
               { Location.filename = ""; line = 0; column = 0; offset = 0 };
               end_pos =
               { Location.filename = ""; line = 0; column = 0; offset = 0 } }
             };
           { Location.value = (Syntax_tree.ExpressionVariable "z");
             location =
             { Location.start_pos =
               { Location.filename = ""; line = 0; column = 0; offset = 0 };
               end_pos =
               { Location.filename = ""; line = 0; column = 0; offset = 0 } }
             }
           ]
         ));
      location =
      { Location.start_pos =
        { Location.filename = ""; line = 0; column = 0; offset = 0 };
        end_pos = { Location.filename = ""; line = 0; column = 0; offset = 0 } }
      }
    |}]

(* Let expressions *)

let%expect_test "simple let" =
  print_endline (show_parsed_expr "let x = 1 in x");
  [%expect
    {|
    { Location.value =
      (Syntax_tree.ExpressionLet (Syntax_tree.Nonrecursive,
         [{ Syntax_tree.binding_pattern =
            { Location.value = (Syntax_tree.PatternVariable "x");
              location =
              { Location.start_pos =
                { Location.filename = ""; line = 0; column = 0; offset = 0 };
                end_pos =
                { Location.filename = ""; line = 0; column = 0; offset = 0 } }
              };
            binding_expression =
            { Location.value =
              (Syntax_tree.ExpressionConstant (Syntax_tree.ConstantInteger 1));
              location =
              { Location.start_pos =
                { Location.filename = ""; line = 0; column = 0; offset = 0 };
                end_pos =
                { Location.filename = ""; line = 0; column = 0; offset = 0 } }
              };
            binding_location =
            { Location.start_pos =
              { Location.filename = ""; line = 0; column = 0; offset = 0 };
              end_pos =
              { Location.filename = ""; line = 0; column = 0; offset = 0 } }
            }
           ],
         { Location.value = (Syntax_tree.ExpressionVariable "x");
           location =
           { Location.start_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 };
             end_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 } }
           }
         ));
      location =
      { Location.start_pos =
        { Location.filename = ""; line = 0; column = 0; offset = 0 };
        end_pos = { Location.filename = ""; line = 0; column = 0; offset = 0 } }
      }
    |}]

let%expect_test "let rec" =
  print_endline (show_parsed_expr "let rec f x = f x in f 1");
  [%expect
    {|
    { Location.value =
      (Syntax_tree.ExpressionLet (Syntax_tree.Recursive,
         [{ Syntax_tree.binding_pattern =
            { Location.value = (Syntax_tree.PatternVariable "f");
              location =
              { Location.start_pos =
                { Location.filename = ""; line = 0; column = 0; offset = 0 };
                end_pos =
                { Location.filename = ""; line = 0; column = 0; offset = 0 } }
              };
            binding_expression =
            { Location.value =
              (Syntax_tree.ExpressionFunction (
                 [{ Location.value = (Syntax_tree.PatternVariable "x");
                    location =
                    { Location.start_pos =
                      { Location.filename = ""; line = 0; column = 0; offset = 0
                        };
                      end_pos =
                      { Location.filename = ""; line = 0; column = 0; offset = 0
                        }
                      }
                    }
                   ],
                 { Location.value =
                   (Syntax_tree.ExpressionApply (
                      { Location.value = (Syntax_tree.ExpressionVariable "f");
                        location =
                        { Location.start_pos =
                          { Location.filename = ""; line = 0; column = 0;
                            offset = 0 };
                          end_pos =
                          { Location.filename = ""; line = 0; column = 0;
                            offset = 0 }
                          }
                        },
                      [{ Location.value = (Syntax_tree.ExpressionVariable "x");
                         location =
                         { Location.start_pos =
                           { Location.filename = ""; line = 0; column = 0;
                             offset = 0 };
                           end_pos =
                           { Location.filename = ""; line = 0; column = 0;
                             offset = 0 }
                           }
                         }
                        ]
                      ));
                   location =
                   { Location.start_pos =
                     { Location.filename = ""; line = 0; column = 0; offset = 0 };
                     end_pos =
                     { Location.filename = ""; line = 0; column = 0; offset = 0 }
                     }
                   }
                 ));
              location =
              { Location.start_pos =
                { Location.filename = ""; line = 0; column = 0; offset = 0 };
                end_pos =
                { Location.filename = ""; line = 0; column = 0; offset = 0 } }
              };
            binding_location =
            { Location.start_pos =
              { Location.filename = ""; line = 0; column = 0; offset = 0 };
              end_pos =
              { Location.filename = ""; line = 0; column = 0; offset = 0 } }
            }
           ],
         { Location.value =
           (Syntax_tree.ExpressionApply (
              { Location.value = (Syntax_tree.ExpressionVariable "f");
                location =
                { Location.start_pos =
                  { Location.filename = ""; line = 0; column = 0; offset = 0 };
                  end_pos =
                  { Location.filename = ""; line = 0; column = 0; offset = 0 } }
                },
              [{ Location.value =
                 (Syntax_tree.ExpressionConstant (Syntax_tree.ConstantInteger 1));
                 location =
                 { Location.start_pos =
                   { Location.filename = ""; line = 0; column = 0; offset = 0 };
                   end_pos =
                   { Location.filename = ""; line = 0; column = 0; offset = 0 } }
                 }
                ]
              ));
           location =
           { Location.start_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 };
             end_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 } }
           }
         ));
      location =
      { Location.start_pos =
        { Location.filename = ""; line = 0; column = 0; offset = 0 };
        end_pos = { Location.filename = ""; line = 0; column = 0; offset = 0 } }
      }
    |}]

(* Conditionals *)

let%expect_test "if then else" =
  print_endline (show_parsed_expr "if true then 1 else 2");
  [%expect
    {|
    { Location.value =
      (Syntax_tree.ExpressionIf (
         { Location.value =
           (Syntax_tree.ExpressionConstant (Syntax_tree.ConstantBoolean true));
           location =
           { Location.start_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 };
             end_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 } }
           },
         { Location.value =
           (Syntax_tree.ExpressionConstant (Syntax_tree.ConstantInteger 1));
           location =
           { Location.start_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 };
             end_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 } }
           },
         (Some { Location.value =
                 (Syntax_tree.ExpressionConstant (Syntax_tree.ConstantInteger 2));
                 location =
                 { Location.start_pos =
                   { Location.filename = ""; line = 0; column = 0; offset = 0 };
                   end_pos =
                   { Location.filename = ""; line = 0; column = 0; offset = 0 } }
                 })
         ));
      location =
      { Location.start_pos =
        { Location.filename = ""; line = 0; column = 0; offset = 0 };
        end_pos = { Location.filename = ""; line = 0; column = 0; offset = 0 } }
      }
    |}]

let%expect_test "if without else" =
  print_endline (show_parsed_expr "if true then ()");
  [%expect
    {|
    File "<string>", line 1, characters 15-15:
    Parse error: Syntax error
    |}]

(* Functions *)

let%expect_test "simple function" =
  print_endline (show_parsed_expr "fun x -> x");
  [%expect
    {|
    { Location.value =
      (Syntax_tree.ExpressionFunction (
         [{ Location.value = (Syntax_tree.PatternVariable "x");
            location =
            { Location.start_pos =
              { Location.filename = ""; line = 0; column = 0; offset = 0 };
              end_pos =
              { Location.filename = ""; line = 0; column = 0; offset = 0 } }
            }
           ],
         { Location.value = (Syntax_tree.ExpressionVariable "x");
           location =
           { Location.start_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 };
             end_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 } }
           }
         ));
      location =
      { Location.start_pos =
        { Location.filename = ""; line = 0; column = 0; offset = 0 };
        end_pos = { Location.filename = ""; line = 0; column = 0; offset = 0 } }
      }
    |}]

let%expect_test "multi-param function" =
  print_endline (show_parsed_expr "fun x y -> x");
  [%expect
    {|
    { Location.value =
      (Syntax_tree.ExpressionFunction (
         [{ Location.value = (Syntax_tree.PatternVariable "x");
            location =
            { Location.start_pos =
              { Location.filename = ""; line = 0; column = 0; offset = 0 };
              end_pos =
              { Location.filename = ""; line = 0; column = 0; offset = 0 } }
            };
           { Location.value = (Syntax_tree.PatternVariable "y");
             location =
             { Location.start_pos =
               { Location.filename = ""; line = 0; column = 0; offset = 0 };
               end_pos =
               { Location.filename = ""; line = 0; column = 0; offset = 0 } }
             }
           ],
         { Location.value = (Syntax_tree.ExpressionVariable "x");
           location =
           { Location.start_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 };
             end_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 } }
           }
         ));
      location =
      { Location.start_pos =
        { Location.filename = ""; line = 0; column = 0; offset = 0 };
        end_pos = { Location.filename = ""; line = 0; column = 0; offset = 0 } }
      }
    |}]

(* Sequences *)

let%expect_test "sequence" =
  print_endline (show_parsed_expr "a; b");
  [%expect
    {|
    { Location.value =
      (Syntax_tree.ExpressionSequence (
         { Location.value = (Syntax_tree.ExpressionVariable "a");
           location =
           { Location.start_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 };
             end_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 } }
           },
         { Location.value = (Syntax_tree.ExpressionVariable "b");
           location =
           { Location.start_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 };
             end_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 } }
           }
         ));
      location =
      { Location.start_pos =
        { Location.filename = ""; line = 0; column = 0; offset = 0 };
        end_pos = { Location.filename = ""; line = 0; column = 0; offset = 0 } }
      }
    |}]

(* Type annotations *)

let%expect_test "type annotation" =
  print_endline (show_parsed_expr "(x : int)");
  [%expect
    {|
    { Location.value =
      (Syntax_tree.ExpressionConstraint (
         { Location.value = (Syntax_tree.ExpressionVariable "x");
           location =
           { Location.start_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 };
             end_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 } }
           },
         { Location.value = (Syntax_tree.TypeConstructor ("int", []));
           location =
           { Location.start_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 };
             end_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 } }
           }
         ));
      location =
      { Location.start_pos =
        { Location.filename = ""; line = 0; column = 0; offset = 0 };
        end_pos = { Location.filename = ""; line = 0; column = 0; offset = 0 } }
      }
    |}]
