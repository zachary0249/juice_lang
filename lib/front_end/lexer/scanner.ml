class scanner source_path =
  object (self)
    val mutable peeked = None
    val mutable line = 1
    val mutable column = 1
    val source = open_in source_path
    method get_line = line
    method get_col = column

    method private input_char_opt ic =
      try Some (input_char ic) with End_of_file -> None

    method peek =
      if peeked |> Option.is_some then peeked
      else
        match self#input_char_opt source with
        | None -> None
        | Some c ->
            peeked <- Some c;
            Some c

    method has_next =
      if peeked |> Option.is_some then true
      else
        match self#input_char_opt source with
        | None -> false
        | Some c ->
            peeked <- Some c;
            true

    method next =
      let nxt =
        if peeked |> Option.is_some then (
          let tmp = peeked in
          peeked <- None;
          tmp)
        else self#input_char_opt source
      in

      match nxt with
      | Some c when c = '\n' ->
          line <- line + 1;
          column <- 1;
          nxt
      | None -> None
      | _ ->
          column <- column + 1;
          nxt
  end
