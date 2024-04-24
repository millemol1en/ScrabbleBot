// From implementation of Trie Dict made for the Yellow Exercise 2b
module internal TrieDictionary
    // The TrieDict is a discriminated union containing a tuple where:
    //      -> fst :: boolean value indicates whether or not it is a leaf
    //      -> snd :: Map holds a character value and all its subsequent connected nodes.
    type TrieDict =
    | TrieNode of bool * Map<char, TrieDict>
    
    let empty () = TrieNode(false, Map.empty)
       
    let insert (s : string) (t : TrieDict) : TrieDict =
        let rec insertHelper (letters : char list) (trieDict : TrieDict) : TrieDict =
            match letters with
            | [] ->
                match trieDict with
                | TrieNode (_, m) -> TrieNode (true, m)
            | cHead :: cTail ->
                match trieDict with
                | TrieNode (b, m) ->
                    match m.TryFind(cHead) with
                    | None   -> TrieNode (b, m.Add(cHead, (insertHelper cTail (empty ()))))  //| Case 1 :: We couldn't find a character in the Map, so 
                    | Some x -> TrieNode (b, m.Add(cHead, (insertHelper cTail x)))          //| Case 2 :: We located a character representing this node 
        
        insertHelper [for c in s -> c] t
    
    
    let lookup (s : string) (t : TrieDict) =
        let rec queryHelper (letters : char list) (trieDict : TrieDict) =
            match letters with
            | [] ->
                match trieDict with
                | TrieNode (b, _) -> b
            | cHead :: cTail ->
                match trieDict with
                | TrieNode (b, m) ->
                    match m.TryFind(cHead) with 
                    | None   -> false
                    | Some x -> queryHelper cTail x
                    
        queryHelper [for c in s -> c] t

    let step (c : char) (t : TrieDict) : (bool * TrieDict) option  =
        match t with
        | TrieNode (_, m) ->
            match m.TryFind(c) with
            | None   -> None
            | Some (TrieNode (bVal, mVal)) -> Some (bVal, TrieNode(bVal, mVal))
          
    ////////////////////////////////
    //         TEST CASES         //
    ////////////////////////////////
    // *Comment out if necessary* //
    let iStrings = ["HELLO"; "HEEHAW"; "HARROW"; "HEAL"; "HEDGE"; "ARROW"; "ARCH"]
    
    let initializeTrie (l : string list) : TrieDict =
        let rec insertListOfStringAcc (sList : string list) (acc : TrieDict) : TrieDict =
            match sList with
            | [] -> acc
            | sHead :: sTail -> insertListOfStringAcc sTail (insert sHead acc)
            
        insertListOfStringAcc l (empty ())