type elt = int
type query = int
type answer = bool
type hash_fn = { a : int; b : int; m : int }
type t = { outer : hash_fn; inner : (hash_fn * elt option array) array }

let p = 1073741789
let hash (h : hash_fn) (x : elt) : int = ((h.a * x) + h.b) mod p mod h.m

exception Collision

let random_hash_fn ~m = { a = 1 + Random.int (p - 1); b = Random.int p; m }

let bucket_sizes (h : hash_fn) (keys : elt array) =
  let counts = Array.make h.m 0 in
  Array.iter
    (fun x ->
      let i = hash h x in
      counts.(i) <- counts.(i) + 1)
    keys;
  counts

let sum_squares = Array.fold_left (fun acc x -> acc + (x * x)) 0

let rec pick_outer keys =
  let n = Array.length keys in
  let h = random_hash_fn ~m:(max 1 n) in
  let sizes = bucket_sizes h keys in
  if sum_squares sizes <= (4 * n) + 1 then (h, sizes) else pick_outer keys

let rec pick_inner bucket =
  let b = Array.length bucket in
  if b = 0 then ({ a = 1; b = 0; m = 1 }, [| None |])
  else
    let m = b * b in
    let h = random_hash_fn ~m in
    let table = Array.make m None in
    try
      Array.iter
        (fun x ->
          let j = hash h x in
          match table.(j) with
          | None -> table.(j) <- Some x
          | Some _ -> raise Collision)
        bucket;
      (h, table)
    with Collision -> pick_inner bucket

let of_seq (seq : elt Seq.t) : t =
  let keys = seq |> List.of_seq |> List.sort_uniq compare |> Array.of_list in
  let outer, sizes = pick_outer keys in
  let groups = Array.map (fun s -> Array.make s 0) sizes in
  let next = Array.make outer.m 0 in
  Array.iter
    (fun x ->
      let i = hash outer x in
      groups.(i).(next.(i)) <- x;
      next.(i) <- next.(i) + 1)
    keys;
  { outer; inner = Array.map pick_inner groups }

let to_iter (t : t) : elt Seq.t =
  Array.to_seq t.inner
  |> Seq.flat_map (fun (_, table) ->
      Array.to_seq table |> Seq.filter_map Fun.id)

let search t x =
  let i = hash t.outer x in
  let h2, bucket = t.inner.(i) in
  let j = hash h2 x in
  match bucket.(j) with None -> false | Some k -> k = x

let combine ans1 ans2 = ans1 || ans2
