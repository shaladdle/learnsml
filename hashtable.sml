signature HASH_KEY =
sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
end

structure IntHashKey =
struct
  type t = int
  fun hash x = x
  fun equal x y = case Int.compare (x, y) of EQUAL => true | _ => false
end

signature HASH_TABLE =
sig
  structure Key : HASH_KEY
  type hashTable
  val new : unit -> hashTable
  val put : hashTable -> Key.t -> string -> unit
  val get : hashTable -> Key.t -> string option
  val del : hashTable -> Key.t -> unit
end

(* hash table implemented using an array of lists *)
(*structure HashTable :> HASH_TABLE =*)
functor HashTable(K : HASH_KEY) :> HASH_TABLE where type Key.t = K.t =
struct
  structure Key : HASH_KEY = K
  type kvPair = Key.t * string
  type bucket = kvPair list

  type hashTable = (bucket ref) Array.array

  val size = 173
  fun hash k = (Key.hash k) mod size

  fun new () = Array.array (size, ref [])

  fun searchBucket [] _ = NONE
    | searchBucket ((k', v')::rest) k = if Key.equal k k'
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
        val without = List.filter (fn (k', _) => not (Key.equal k k')) (!bucket)
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
