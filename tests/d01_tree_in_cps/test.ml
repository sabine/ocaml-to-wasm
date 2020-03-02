type 'a binary_tree =
  | Empty
  | Node of 'a * ('a binary_tree) * ('a binary_tree)

let rec tree_height t k = match t with
  | Empty -> k 0
  | Node (_, l, r) ->
      tree_height l (fun lh ->
      tree_height r (fun rh ->
      k (1 + max lh rh)))


