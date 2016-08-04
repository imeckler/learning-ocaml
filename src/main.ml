open Core.Std
open Async.Std
  
let rec loop i f =
  if i = 0
  then ()
  else begin
    f ();
    loop (i-1) f
  end

module Command = struct
  module T = struct
    type t = 
      | Set of string * Fruit.t
      | Get of string
      [@@deriving sexp]
  end
  include Sexpable.To_stringable(T)
  include T

  let exec cmd fruits = 
    match cmd with
    | Set (key, Apple) -> 
        loop 1000 (fun () -> print_endline "fuck you"); 
        print_endline "fruit time";
        fruits
    | Set (key, fruit) -> Map.add fruits ~key ~data:fruit
    | Get key -> 
        begin match Map.find fruits key with
        | None -> print_endline " Shit!"
        | Some fruit -> printf !"%{Fruit}\n" fruit
        end; 
        fruits
end

let stdin = Lazy.force Reader.stdin

let save_fruits fruits = 
  Writer.save_sexp 
    "fruits.sexp" 
    ([%sexp_of:Fruit.t String.Map.t] fruits)

let maybe_exit fruits = 
  if Map.length fruits < 5
  then begin 
    print_endline "dontcha wanna add more fruits?"; 
    return fruits 
  end else begin
    let fruit_list = Map.to_alist fruits in
    let fruits_string = 
      String.concat ~sep:", "
        (List.map fruit_list ~f:(fun (k, v) -> 
          sprintf !"(%s, %{Fruit})" k v))
    in
    print_endline fruits_string; 
    let%bind () = save_fruits fruits in
    exit 0
  end

let get_initial_fruits () = 
  match%bind Reader.load_sexp "fruits.sexp" [%of_sexp:Fruit.t String.Map.t] with
  | Error _ -> 
      print_endline "where's ya fruits? huh?"; 
      print_endline "how much ya like fruits?"; 
      let%bind `Ok line = Reader.read_line stdin in
      if Int.of_string line > 1000 
      then return String.Map.empty
      else begin
        print_endline "well fuck you"; 
        exit 0
      end
  | Ok fruits -> return fruits 

let main () = 
  print_endline "  FFF  RRR  U U  III  TTT              ";
  print_endline "  F    R R  U U   I    T               ";
  print_endline "  FFF  RR   U U   I    T               ";
  print_endline "  F    R R  U U   I    T               ";
  print_endline "  F    R R  UUU  III   T   spells fruit";
  let f fruits line =
    match line with
    | "" -> maybe_exit fruits
    | _ -> 
        return begin match Command.of_string line with
        | exception e -> print_endline "Shit!"; fruits
        | cmd -> Command.exec cmd fruits
        end
  in
  let%bind fruits = get_initial_fruits () in
  print_endline "fruit time";
  let%map _ = 
    Pipe.fold ~f ~init:fruits
      (Reader.lines stdin)
  in
  ()

let () = 
  don't_wait_for (main ());
  never_returns (Scheduler.go ())

