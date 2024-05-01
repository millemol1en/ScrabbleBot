// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a : comparison> = R of Map<'a, uint32> 

    let empty : MultiSet<'a> = R Map.empty

    let isEmpty (R mySet : MultiSet<'a>): bool = Map.isEmpty mySet

    let size (R mySet) = 
        Map.fold (fun acc _ elem -> acc + elem) 0u mySet

    let contains (a : 'a) (R mySet) : bool =
        Map.containsKey a mySet
        
    let numItems (a : 'a) (R mySet : MultiSet<'a>) =
        match Map.tryFind a mySet with
        | None -> (uint32 0)
        | Some value -> value
    let add (a : 'a) (n : uint32) (R mySet : MultiSet<'a>) : MultiSet<'a> =
        let numbers = n + (numItems a (R mySet))
        R (Map.add a numbers mySet)

    let addSingle (a : 'a) (R mySet : MultiSet<'a>) : MultiSet<'a> =
        add a (uint32 1) (R mySet)

    // val remove : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a>
    let remove (a : 'a) (n : uint32) (R mySet  : MultiSet<'a>) : MultiSet<'a> =
        let (numberOfOccurrences : uint32) = numItems a (R (mySet))
        if numberOfOccurrences > n 
            then
                R ((Map.remove a mySet).Add(a, (numberOfOccurrences - n)))
            else 
                R (mySet.Remove a) 
         //? finds the value of the given key.


    let removeSingle (a : 'a) (mySet : MultiSet<'a>) : MultiSet<'a> = 
        remove a 1u mySet

    let fold (f : 'b -> 'a -> uint32 -> 'b) (x : 'b) (R mySet : MultiSet<'a>) = 
        Map.fold f x mySet

    let foldBack (f : 'a -> uint32 -> 'b -> 'b) (R mySet : MultiSet<'a>) (x : 'b) =
        Map.foldBack f mySet x

    let ofList (lst : 'a list) : MultiSet<'a> = List.fold (fun acc elem -> addSingle elem acc) empty lst
    let toList s = foldBack (fun elem num acc -> List.init (int32 num) (fun _ -> elem) @ acc) s []


    let map (_ : 'a -> 'b) (_ : MultiSet<'a>) : MultiSet<'b> = failwith "not done"

    let union (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = failwith "not done"
    let sum (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = failwith "not done"
    
    let subtract (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = failwith "not done"
    
    let intersection (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = failwith "not done"