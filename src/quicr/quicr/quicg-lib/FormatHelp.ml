type escape_t =
| Text
| Dot
| Latex
| HTML


let fmt_conj escape ff () =
  Format.fprintf ff "%s"
  (
    match escape with
    | Text -> "/\\"
    | Dot -> "/\\\\"
    | Latex -> "\\wedge"
    | HTML -> "\\wedge"
  )

let fmt_disj escape ff () =
  Format.fprintf ff "%s"
  (
    match escape with
    | Text -> "\\/"
    | Dot -> "\\\\/"
    | Latex -> "\\vee"
    | HTML -> "\\vee"
  )

let fmt_eq _escape ff () =
  Format.fprintf ff "="

let fmt_lt escape ff () =
  Format.fprintf ff "%s"
  (
    match escape with
    | Text -> "<"
    | Dot -> "<"
    | Latex -> "<"
    | HTML -> "&lt;"
  )
  
let fmt_gt escape ff () =
  Format.fprintf ff "%s"
  (
    match escape with
    | Text -> ">"
    | Dot -> ">"
    | Latex -> ">"
    | HTML -> "&gt;"
  )

let fmt_le escape ff () =
  Format.fprintf ff "%s"
  (
    match escape with
    | Text -> "<="
    | Dot -> "<="
    | Latex -> "\\leq"
    | HTML -> "\\leq"
  )

let fmt_ge escape ff () =
  Format.fprintf ff "%s"
  (
    match escape with
    | Text -> ">="
    | Dot -> ">="
    | Latex -> "\\geq"
    | HTML -> "\\geq"
  )

let fmt_ne escape ff () =
  Format.fprintf ff "%s"
  (
    match escape with
    | Text -> "!="
    | Dot -> "!="
    | Latex -> "\\not="
    | HTML -> "\\not="
  )

let fmt_top escape ff () =
  Format.fprintf ff "%s"
  (
    match escape with
    | Text -> "Top"
    | Dot -> "Top"
    | Latex -> "\\top"
    | HTML -> "\\top"
  )

let fmt_bottom escape ff () =
  Format.fprintf ff "%s"
    (
      match escape with
      | Text -> "Bottom"
      | Dot -> "Bottom"
      | Latex -> "\\bot"
      | HTML -> "\\bot"
    )

let fmt_plus _escape ff () =
  Format.fprintf ff "+"

let fmt_minus _escape ff () =
  Format.fprintf ff "-"

let fmt_mul escape ff () =
  Format.fprintf ff "%s"
    (
      match escape with
      | Text -> "*"
      | Dot -> "*"
      | Latex -> " \\times "
      | HTML -> " \\times "
    )

let fmt_leftbr escape ff () =
  Format.fprintf ff "%s"
    (
      match escape with
      | Text -> "{"
      | Dot -> "{"
      | Latex -> "\\left\\{"
      | HTML -> "\\left\\{"
    )

let fmt_rightbr escape ff () =
  Format.fprintf ff "%s"
    (
      match escape with
      | Text -> "}"
      | Dot -> "}"
      | Latex -> "\\right\\}"
      | HTML -> "\\right\\}"
    )

    

let fmt_union escape ff () =
  Format.fprintf ff "%s"
    (
      match escape with
      | Text -> "U"
      | Dot -> "U"
      | Latex -> "\\cup"
      | HTML -> "\\cup"
    )
  
let fmt_inter escape ff () =
  Format.fprintf ff "%s"
    (
      match escape with
      | Text -> "^"
      | Dot -> "^"
      | Latex -> "\\cap"
      | HTML -> "\\cap"
    )

let fmt_emptyset escape ff () =
  Format.fprintf ff "%s"
    (
      match escape with
      | Text -> "{}"
      | Dot -> "{}"
      | Latex -> "\\emptyset"
      | HTML -> "\\emptyset"
    )

let fmt_universeset escape ff () =
  Format.fprintf ff "%s"
    (
      match escape with
      | Text -> "[U]"
      | Dot -> "[U]"
      | Latex -> "\\Sigma"
      | HTML -> "\\Sigma"
    )

let fmt_leftparen escape ff () =
  Format.fprintf ff "%s"
    (
      match escape with
      | Text -> "("
      | Dot -> "("
      | Latex -> "\\left("
      | HTML -> "\\left("
    )

let fmt_rightparen escape ff () =
  Format.fprintf ff "%s"
    (
      match escape with
      | Text -> ")"
      | Dot -> ")"
      | Latex -> "\\right)"
      | HTML -> "\\right)"
    )

let fmt_subseteq escape ff () =
  Format.fprintf ff "%s"
    (
      match escape with
      | Text -> "<="
      | Dot -> "<="
      | Latex -> "\\subseteq"
      | HTML -> "\\subseteq"
    )


let fmt_bar escape ff () =
  Format.fprintf ff "%s"
    (
      match escape with
      | Text -> "|"
      | Dot -> "|"
      | Latex -> " \\, \\middle| \\, "
      | HTML -> " \\, \\middle| \\, "
    )

let fmt_in escape ff () =
  Format.fprintf ff "%s"
    (
      match escape with
      | Text -> "in"
      | Dot -> "in"
      | Latex -> "\\in"
      | HTML -> "\\in"
    )

let fmt_extra_var escape ff () =
  Format.fprintf ff "%s"
    (
      match escape with
      | Text -> "$x"
      | Dot -> "$x"
      | Latex -> "\\omega"
      | HTML -> "\\omega"
    )
  

let fmt_newline escape ff () =
  match escape with
    | Text -> Format.fprintf ff "@."
    | Dot -> Format.fprintf ff "@."
    | Latex -> Format.fprintf ff "\\\\"
    | HTML -> Format.fprintf ff "\\\\"

let fmt_array escape fmt_cont ff cont =
  match escape with
    | Text -> fmt_cont ff ()
    | Dot -> fmt_cont ff ()
    | Latex ->
        Format.fprintf ff "\\begin{array}{l} %a \\end{array}" fmt_cont ()
    | HTML ->
        Format.fprintf ff "\\begin{array}{l} %a \\end{array}" fmt_cont ()

        
let fmt_set_comp escape fmt_lhs fmt_rhs ff () =
  fmt_leftbr escape ff ();
  fmt_array escape fmt_lhs ff ();
  fmt_bar escape ff ();
  fmt_array escape fmt_rhs ff ();
  fmt_rightbr escape ff ();
