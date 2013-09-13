signature HASH_TABLE =
sig
  type hashTable
  val new : unit -> hashTable
  val put : hashTable -> int -> string -> unit
  val get : hashTable -> int -> string option
  val del : hashTable -> int -> unit
end

(* hash table implemented using an array of lists *)
structure HashTable :> HASH_TABLE =
struct
  type kvPair = (int * string)
  type bucket = kvPair list

  type hashTable = (bucket ref) Array.array

  val size = 173
  fun hash k = k mod size

  fun new () = Array.array (size, ref [])

  fun searchBucket [] _ = NONE
    | searchBucket ((k', v')::rest) k = if k = k'
                                        then SOME (k', v')
                                        else searchBucket rest k

  fun get ht k = let 
        val bucket = Array.sub (ht, hash k)
      in
        case searchBucket (!bucket) k of 
               SOME(_, v) => SOME v
             | NONE => NONE
      end

  fun del ht k = let 
        val i = hash k
        val bucket = Array.sub (ht, i)
        val without = List.filter (fn (k', _) => k' <> k) (!bucket)
      in
        Array.update (ht, i, ref without)
      end

  fun put ht k v = (del ht k; let
                      val i = hash k
                      val bucket = Array.sub (ht, i)
                      val newBucket = ((k, v)::(!bucket))
                    in
                      Array.update (ht, i, ref newBucket)
                    end)

end
