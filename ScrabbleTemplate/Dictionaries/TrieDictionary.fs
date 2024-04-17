module internal TrieDictionary

    // The trie is a tuple:
    //      -> fst :: boolean value indicates whether or not it is a leaf
    //      -> snd :: Map holds a character value and all its subsequent connected nodes.
    type TrieDict =
    | TrieNode of bool * Map<char, TrieDict>
      
    let emptyTrie = TrieNode(false, Map.empty)
       
    let iStrings = ["HELLO"; "HEEHAW"; "HARROW"; "HEAL"; "HEDGE"; "ARROW"; "ARCH"]
        
    
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
                    | None   -> TrieNode (b, m.Add(cHead, (insertHelper cTail emptyTrie)))  //| Case 1 :: We couldn't find a character in the Map, so 
                    | Some x -> TrieNode (b, m.Add(cHead, (insertHelper cTail x)))          //| Case 2 :: We located a character representing this node 
        
        insertHelper [for c in s -> c] t
    
    
    let query (s : string) (t : TrieDict) =
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

    let initializeTrie (l : string list) : TrieDict =
        let rec insertListOfStringAcc (sList : string list) (acc : TrieDict) : TrieDict =
            match sList with
            | [] -> acc
            | sHead :: sTail -> insertListOfStringAcc sTail (insert sHead acc)
            
        insertListOfStringAcc l emptyTrie
    
    let step c dict =
        let aux =
            function
            | c1, TrieNode (b, m) ->
                match m.TryFind(c1) with
                | None                -> None
                | Some (TrieNode(bo, m1)) -> Some (bo, TrieNode(bo, m1))
        aux (c, dict)