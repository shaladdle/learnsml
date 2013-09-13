signature HASH_KEY =
sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
end

structure IntKey =
struct
  type t = int
  fun hash x = x
  fun equal x y = case Int.compare (x, y) of EQUAL => true | _ => false
end

structure NaiveStringKey =
struct
  type t = string

  (* This helper is fairly unrelated to the structure *)
  local
    fun sum il = List.foldl (fn (x, y) => x + y) 0 il
  in
    fun hash s = sum ((map ord) (String.explode s))
  end

  fun equal x y = case String.compare (x, y) of EQUAL => true | _ => false
end

signature HASH_TABLE =
sig
  type key
  type 'a hashTable
  val new : unit -> 'a hashTable
  val put : 'a hashTable -> key -> 'a -> unit
  val get : 'a hashTable -> key -> 'a option
  val del : 'a hashTable -> key -> unit
end

functor HashTable(K : HASH_KEY) :> HASH_TABLE where type key = K.t =
struct
  type key = K.t
  type 'a kvPair = K.t * 'a
  type 'a bucket = 'a kvPair list

  type 'a hashTable = 'a bucket Array.array

  val size = 173
  fun hash k = (K.hash k) mod size

  fun new () = Array.array (size, [])

  fun searchBucket [] _ = NONE
    | searchBucket ((k', v')::rest) k = if K.equal k k'
                                        then SOME (k', v')
                                        else searchBucket rest k

  fun get ht k = let 
        val bucket = Array.sub (ht, hash k)
      in
        case searchBucket bucket k of 
               SOME(_, v) => SOME v
             | NONE => NONE
      end

  fun del ht k = let 
        val i = hash k
        val bucket = Array.sub (ht, i)
        val without = List.filter (fn (k', _) => not (K.equal k k')) bucket
      in
        Array.update (ht, i, without)
      end

  fun put ht k v = (del ht k; let
                      val i = hash k
                      val bucket = Array.sub (ht, i)
                      val newBucket = (k, v)::bucket
                    in
                      Array.update (ht, i, newBucket)
                    end)

end
