open Test_helpers.Helpers

let%expect_test "record literal parsing" =
  print_endline (show_parsed_expr "{ x = 1; y = 2 }");
  [%expect{|
    { Location.value =
      (Syntax_tree.ExpressionRecord
         [{ Syntax_tree.field_name =
            { Location.value = "x";
              location =
              { Location.start_pos =
                { Location.filename = ""; line = 0; column = 0; offset = 0 };
                end_pos =
                { Location.filename = ""; line = 0; column = 0; offset = 0 } }
              };
            field_value =
            { Location.value =
              (Syntax_tree.ExpressionConstant (Syntax_tree.ConstantInteger 1));
              location =
              { Location.start_pos =
                { Location.filename = ""; line = 0; column = 0; offset = 0 };
                end_pos =
                { Location.filename = ""; line = 0; column = 0; offset = 0 } }
              }
            };
           { Syntax_tree.field_name =
             { Location.value = "y";
               location =
               { Location.start_pos =
                 { Location.filename = ""; line = 0; column = 0; offset = 0 };
                 end_pos =
                 { Location.filename = ""; line = 0; column = 0; offset = 0 } }
               };
             field_value =
             { Location.value =
               (Syntax_tree.ExpressionConstant (Syntax_tree.ConstantInteger 2));
               location =
               { Location.start_pos =
                 { Location.filename = ""; line = 0; column = 0; offset = 0 };
                 end_pos =
                 { Location.filename = ""; line = 0; column = 0; offset = 0 } }
               }
             }
           ]);
      location =
      { Location.start_pos =
        { Location.filename = ""; line = 0; column = 0; offset = 0 };
        end_pos = { Location.filename = ""; line = 0; column = 0; offset = 0 } }
      }
    |}]

let%expect_test "record access parsing" =
  print_endline (show_parsed_expr "r.field");
  [%expect{|
    { Location.value =
      (Syntax_tree.ExpressionRecordAccess (
         { Location.value = (Syntax_tree.ExpressionVariable "r");
           location =
           { Location.start_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 };
             end_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 } }
           },
         "field"));
      location =
      { Location.start_pos =
        { Location.filename = ""; line = 0; column = 0; offset = 0 };
        end_pos = { Location.filename = ""; line = 0; column = 0; offset = 0 } }
      }
    |}]

let%expect_test "record update parsing" =
  print_endline (show_parsed_expr "{ r with x = 10 }");
  [%expect{|
    { Location.value =
      (Syntax_tree.ExpressionRecordUpdate (
         { Location.value = (Syntax_tree.ExpressionVariable "r");
           location =
           { Location.start_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 };
             end_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 } }
           },
         [{ Syntax_tree.field_name =
            { Location.value = "x";
              location =
              { Location.start_pos =
                { Location.filename = ""; line = 0; column = 0; offset = 0 };
                end_pos =
                { Location.filename = ""; line = 0; column = 0; offset = 0 } }
              };
            field_value =
            { Location.value =
              (Syntax_tree.ExpressionConstant (Syntax_tree.ConstantInteger 10));
              location =
              { Location.start_pos =
                { Location.filename = ""; line = 0; column = 0; offset = 0 };
                end_pos =
                { Location.filename = ""; line = 0; column = 0; offset = 0 } }
              }
            }
           ]
         ));
      location =
      { Location.start_pos =
        { Location.filename = ""; line = 0; column = 0; offset = 0 };
        end_pos = { Location.filename = ""; line = 0; column = 0; offset = 0 } }
      }
    |}]

let%expect_test "match expression parsing" =
  print_endline (show_parsed_expr "match x with | 0 -> 1 | n -> n");
  [%expect{|
    { Location.value =
      (Syntax_tree.ExpressionMatch (
         { Location.value = (Syntax_tree.ExpressionVariable "x");
           location =
           { Location.start_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 };
             end_pos =
             { Location.filename = ""; line = 0; column = 0; offset = 0 } }
           },
         [{ Syntax_tree.arm_pattern =
            { Location.value =
              (Syntax_tree.PatternConstant (Syntax_tree.ConstantInteger 0));
              location =
              { Location.start_pos =
                { Location.filename = ""; line = 0; column = 0; offset = 0 };
                end_pos =
                { Location.filename = ""; line = 0; column = 0; offset = 0 } }
              };
            arm_guard = None;
            arm_expression =
            { Location.value =
              (Syntax_tree.ExpressionConstant (Syntax_tree.ConstantInteger 1));
              location =
              { Location.start_pos =
                { Location.filename = ""; line = 0; column = 0; offset = 0 };
                end_pos =
                { Location.filename = ""; line = 0; column = 0; offset = 0 } }
              };
            arm_location =
            { Location.start_pos =
              { Location.filename = ""; line = 0; column = 0; offset = 0 };
              end_pos =
              { Location.filename = ""; line = 0; column = 0; offset = 0 } }
            };
           { Syntax_tree.arm_pattern =
             { Location.value = (Syntax_tree.PatternVariable "n");
               location =
               { Location.start_pos =
                 { Location.filename = ""; line = 0; column = 0; offset = 0 };
                 end_pos =
                 { Location.filename = ""; line = 0; column = 0; offset = 0 } }
               };
             arm_guard = None;
             arm_expression =
             { Location.value = (Syntax_tree.ExpressionVariable "n");
               location =
               { Location.start_pos =
                 { Location.filename = ""; line = 0; column = 0; offset = 0 };
                 end_pos =
                 { Location.filename = ""; line = 0; column = 0; offset = 0 } }
               };
             arm_location =
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
