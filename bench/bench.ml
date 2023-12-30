module ListTakeDrop : sig
  val take : int -> 'a list -> 'a list
  val take_kit : int -> 'a list -> 'a list
  val take_kit2 : int -> 'a list -> 'a list
  val take_con : int -> 'a list -> 'a list
  val take_base : int -> 'a list -> 'a list
  val drop_kit : int -> 'a list -> 'a list
  val drop_con : int -> 'a list -> 'a list
  val drop_base : int -> 'a list -> 'a list
  val take_while_kit : ('a -> bool) -> 'a list -> 'a list
  val take_while_con : ('a -> bool) -> 'a list -> 'a list
  val take_while_base : ('a -> bool) -> 'a list -> 'a list
  val drop_while_kit : ('a -> bool) -> 'a list -> 'a list
  val drop_while_con : ('a -> bool) -> 'a list -> 'a list
  val drop_while_base : ('a -> bool) -> 'a list -> 'a list
end
=
struct
  let take n l =
    if n < 0 then invalid_arg "List.take"
    else
      let[@tail_mod_cons] rec loop n l =
        if n = 0 then []
        else
          match l with
          | [] -> []
          | x::l -> x::loop (n - 1) l
      in loop n l

  let take_kit n l =
    let rec aux i acc = function
      | x::l when i < n -> aux (i + 1) (x::acc) l
      | _rest -> List.rev acc
    in
    if n < 0 then invalid_arg "List.take";
    aux 0 [] l

  let take_kit2 n l =
    let[@tail_mod_cons] rec aux n l =
      match n, l with
      | 0, _ | _, [] -> []
      | n, x::l -> x::aux (n - 1) l
    in
    if n < 0 then invalid_arg "List.take";  
    aux n l

  (* Derivate of: https://github.com/c-cube/ocaml-containers/blob/60bb2c8c68e3fce3d77c0e521fd6a1861ce6701e/src/core/CCList.ml#L797-L804 *)
  let[@tail_mod_cons] rec take_con_nonneg n l =
    match l with
    | [] -> []
    | x :: l' ->
      if n > 0 then
        x :: take_con_nonneg (n - 1) l'
      else
        []
  let take_con n l =
    if n < 0 then invalid_arg "List.take"
    else take_con_nonneg n l

  (* Derivative of: https://github.com/janestreet/base/blob/436bc5f284f55dcc99fd34cb772dd2ce0482b83d/src/list.ml#L1325-L1336 *)
  let take_base n t_orig =
    if n < 0
    then invalid_arg "List.take"
    else if n = 0
    then []
    else (
      let rec loop n t accum =
        match t with
        | [] -> t_orig
        | hd :: tl -> if n = 0 then List.rev accum else loop (n - 1) tl (hd :: accum)
      in
      loop n t_orig [])

  let drop_kit n l =
    let rec aux i = function
      | _x::l when i < n -> aux (i + 1) l
      | rest -> rest
    in
    if n < 0 then invalid_arg "List.drop";
    aux 0 l

  (* Derivative of: https://github.com/c-cube/ocaml-containers/blob/60bb2c8c68e3fce3d77c0e521fd6a1861ce6701e/src/core/CCList.ml#L808C1-L812 *)
  let rec drop_con_nonneg n l =
    match l with
    | [] -> []
    | _ when n = 0 -> l
    | _ :: l' -> drop_con_nonneg (n - 1) l'
  let drop_con n l =
    if n < 0 then invalid_arg "List.drop"
    else drop_con_nonneg n l
  
  (* Derivative of: https://github.com/janestreet/base/blob/436bc5f284f55dcc99fd34cb772dd2ce0482b83d/src/list.ml#L1338-L1342 *)
  let rec drop_base n t =
    if n < 0 then invalid_arg "List.drop"
    else
      match t with
      | _ :: tl when n > 0 -> drop_base (n - 1) tl
      | t -> t

  let take_while_kit p l =
    let rec aux acc = function
      | x::l when p x -> aux (x::acc) l
      | _rest -> List.rev acc
    in
    aux [] l

  (* Source: https://github.com/c-cube/ocaml-containers/blob/60bb2c8c68e3fce3d77c0e521fd6a1861ce6701e/src/core/CCList.ml#L897C1-L904C9 *)
  let[@tail_mod_cons] rec take_while_con p l =
    match l with
    | [] -> []
    | x :: l' ->
      if p x then
        x :: take_while_con p l'
      else
        []

  (* Derivative of: https://github.com/janestreet/base/blob/436bc5f284f55dcc99fd34cb772dd2ce0482b83d/src/list.ml#L1364-L1371 *)
  let take_while_base f xs =
    let rec loop acc = function
      | hd :: tl when f hd -> loop (hd :: acc) tl
      | _ -> List.rev acc
    in
    loop [] xs [@nontail]
  
  let rec drop_while_kit p = function
    | x::l when p x -> drop_while_kit p l
    | rest -> rest

  (* https://github.com/c-cube/ocaml-containers/blob/60bb2c8c68e3fce3d77c0e521fd6a1861ce6701e/src/core/CCList.ml#L908-L915 *)
  let rec drop_while_con p l =
    match l with
    | [] -> []
    | x :: l' ->
      if p x then
        drop_while_con p l'
      else
        l

  (* Derivative of: https://github.com/janestreet/base/blob/436bc5f284f55dcc99fd34cb772dd2ce0482b83d/src/list.ml#L1373C3-L1377 *)
  let rec drop_while_base f t =
    match t with
    | hd :: tl when f hd -> drop_while_base f tl
    | t -> t
end;;

let test
  ~take
  ~drop
  ~take_while
  ~drop_while
=
  assert (take 3 [1; 2; 3; 4; 5] = [1; 2; 3]);
  assert (take 3 [1; 2] = [1; 2]);
  assert (take 3 [] = []);
  assert ((try take (-1) [1; 2] with Invalid_argument _ -> [999]) = [999]);
  assert (take 0 [1; 2] = []);
  assert (drop 3 [1; 2; 3; 4; 5] = [4; 5]);
  assert (drop 3 [1; 2] = []);
  assert (drop 3 [] = []);
  assert ((try drop (-1) [1; 2] with Invalid_argument _ -> [999]) = [999]);
  assert (drop 0 [1; 2] = [1; 2]);
  assert (take_while (fun x -> x < 3) [1; 2; 3; 4; 1; 2; 3; 4]
        = [1; 2]);
  assert (take_while (fun x -> x < 9) [1; 2; 3] = [1; 2; 3]);
  assert (take_while (fun x -> x < 0) [1; 2; 3] = []);
  assert (drop_while (fun x -> x < 3) [1; 2; 3; 4; 5; 1; 2; 3]
        = [3; 4; 5; 1; 2; 3]);
  assert (drop_while (fun x -> x < 9) [1; 2; 3] = []);
  assert (drop_while (fun x -> x < 0) [1; 2; 3] = [1; 2; 3]);
;;

let () = test 
  ~take:ListTakeDrop.take_kit 
  ~drop:ListTakeDrop.drop_kit
  ~take_while:ListTakeDrop.take_while_kit
  ~drop_while:ListTakeDrop.drop_while_kit
in
let () = test 
  ~take:ListTakeDrop.take_kit2
  ~drop:ListTakeDrop.drop_kit
  ~take_while:ListTakeDrop.take_while_kit
  ~drop_while:ListTakeDrop.drop_while_kit
in
let () = test 
  ~take:ListTakeDrop.take_con 
  ~drop:ListTakeDrop.drop_con
  ~take_while:ListTakeDrop.take_while_con
  ~drop_while:ListTakeDrop.drop_while_con
in
let () = test 
  ~take:ListTakeDrop.take
  ~drop:ListTakeDrop.drop_con
  ~take_while:ListTakeDrop.take_while_con
  ~drop_while:ListTakeDrop.drop_while_con
in
let () = test 
  ~take:ListTakeDrop.take_base 
  ~drop:ListTakeDrop.drop_base
  ~take_while:ListTakeDrop.take_while_base
  ~drop_while:ListTakeDrop.drop_while_base
in ()

type sys_data = {
  timestamp: float;
  minor_heap_words: float;
  major_heap_words: float;
};;

let capture_sys_data (_: unit): sys_data =
  let stat = Gc.quick_stat() in
  {
    timestamp = Sys.time();
    minor_heap_words = stat.minor_words;
    major_heap_words = stat.major_words;
  }
;;

type report = {
  fun_name: string;
  fun_impl: string;
  list_el_type: string;
  list_size: int;
  start_stop_index: int option;
  time_delta: float;
  alloc_delta: float;
}

let serialize_report (rep: report) : string =
  "{\"fun_name\": \"" ^ rep.fun_name ^ "\", "
  ^ "\"fun_impl\": \"" ^ rep.fun_impl ^ "\", "
  ^ "\"list_el_type\": \"" ^ rep.list_el_type ^ "\", "
  ^ "\"list_size\": \"" ^ (Int.to_string rep.list_size) ^ "\", "
  ^ "\"start_stop_index\": \"" ^ (
    match rep.start_stop_index with
    | Some x -> (Int.to_string x)
    | None -> "null"
  ) ^ "\", "
  ^ "\"time_delta\": \"" ^ (Float.to_string rep.time_delta) ^ "\", "
  ^ "\"alloc_delta\": \"" ^ (Float.to_string rep.alloc_delta) ^ "\"}"
;;

let prof
  (n: int)
  ~(fun_name: string)
  ~(fun_impl: string)
  ~(list_el_type: string)
  ~(list_size: int)
  ~(start_stop_index: int option)
  (f: unit -> 'a) 
  : report 
=
  let (sum_time_delta, sum_alloc_delta) = 
    List.init n (fun _ -> 
      let sys_data_t0 = capture_sys_data() in
      let _ = f() in
      let sys_data_t1 = capture_sys_data() in
      (
        sys_data_t1.timestamp -. sys_data_t0.timestamp,
        (sys_data_t1.major_heap_words +. sys_data_t1.minor_heap_words)
        -. (sys_data_t0.major_heap_words +. sys_data_t0.minor_heap_words)
      )
    )
    |> List.fold_left 
      (fun (sum_t, sum_alloc) (t, alloc) -> (sum_t +. t, sum_alloc +. alloc))
      (0.0, 0.0)
  in
  {
    fun_name = fun_name;
    fun_impl = fun_impl;
    list_el_type = list_el_type;
    list_size = list_size;
    start_stop_index = start_stop_index;
    time_delta = sum_time_delta /. (Float.of_int n);
    alloc_delta = sum_alloc_delta /. (Float.of_int n);
  }
;;

let rec range 
  (start : int) 
  (stop : int) 
  (step : int)
= fun () -> (
  if step = 0 then
    Seq.Nil
  else if start > stop then
    Seq.Nil
  else
    Seq.Cons (start, range (start + step) stop step)
);;

let linspace
  (start : int)
  (stop : int)
  (n : int)
  : int Seq.t
=
  let step = (stop - start) / (n - 1) in
  range start stop step
;;

let mk_char_list n =
  range 0 n 1 |> Seq.map (fun n -> Char.chr (n mod 128)) |> List.of_seq
;;

let bench_take_reports =
  List.map
    (fun (fun_name, fun_impl, take) ->
      Seq.map
        (fun list_size -> 
          let start_stop_index = list_size / 2 in
          let char_list = mk_char_list list_size in
          prof
            10
            ~fun_name
            ~fun_impl
            ~list_el_type:"char"
            ~list_size:list_size
            ~start_stop_index:(Some start_stop_index)
            (fun () -> 
              take
                start_stop_index
                char_list
            )
        )
        (linspace 0 100_000 1000)
      |> List.of_seq
    )
    [
      ("take", "Containers", ListTakeDrop.take_con);
      ("take", "kit-ty-kate", ListTakeDrop.take_kit);
      ("take", "kit-ty-kate 2.", ListTakeDrop.take_kit2);
      ("take", "take-custom-mod-cons", ListTakeDrop.take);
      ("take", "Base", ListTakeDrop.take_base);
    ]
  |> List.concat
in
let bench_drop_reports =
  List.map
    (fun (fun_name, fun_impl, take) ->
      Seq.map
        (fun list_size -> 
          let start_stop_index = list_size / 2 in
          let char_list = mk_char_list list_size in
          prof
            10
            ~fun_name
            ~fun_impl
            ~list_el_type:"char"
            ~list_size:list_size
            ~start_stop_index:(Some start_stop_index)
            (fun () -> 
              take
                start_stop_index
                char_list
            )
        )
        (linspace 0 100_000 1000)
      |> List.of_seq
    )
    [
      ("drop", "Containers", ListTakeDrop.drop_con);
      ("drop", "kit-ty-kate", ListTakeDrop.drop_kit);
      ("drop", "Base", ListTakeDrop.drop_base);
    ]
  |> List.concat
in
  let oc = open_out "./reports.jsonl" in
  let _ = List.map
    (fun report ->
      output_string oc (serialize_report report ^ "\n")
    )
    (bench_take_reports @ bench_drop_reports)
  in
  close_out oc
;;

