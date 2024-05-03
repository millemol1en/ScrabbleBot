module PlayMaker

    open MultiSet
    open ScrabbleUtil.Dictionary
    open StateMonad
    open Parser
    
    // 01. Type declaration:
    type coord = int * int
    
    type Direction =
        | Up
        | Down
        | Left
        | Right
    
    
    // 00.
    type state = { // copy-&-pasta of state ... no good
        board          : Parser.board
        dict           : ScrabbleUtil.Dictionary.Dict
        playerNumber   : uint32                          
        hand           : MultiSet.MultiSet<uint32>       
        numPlayers     : uint32                          
        playerTurn     : uint32                          
        timeout        : uint32 option                   
        lettersOnBoard : Map<coord, (char * int)>        
    }
    
    // 02. As MultiSets define our hand using the uint32 identifier, we use the following method to convert between characters and their unique ID
    //     Theory for this is given here - [https://stackoverflow.com/questions/29638419/whats-the-theory-behind-subtracting-from-toupper]
    let charToMultiSetID (c : char) = uint32(System.Char.ToUpper(c)) - 64u
    
    let multiSetIdToChar (msID : uint32) = char(msID + 64u)
    
    let multisetToChar = MultiSet.map (fun msID -> multiSetIdToChar msID)
    
    let collectTileInfo (pieceCoordinate : coord) (pieceChar : char) (piecePoint : int) : (coord * uint32 * char * int) =
        (pieceCoordinate, (charToMultiSetID pieceChar), pieceChar, piecePoint)
    
    // 03.
    let stepDir (dir : Direction) ((x, y) : coord) : coord =
        match dir with
        | Up -> (x, y - 1)
        | Right  -> (x + 1, y)
        | Down -> (x, y + 1)
        | Left  -> (x - 1, y)
        
    let doesTileExist (st : state) (c : coord) =
        match (st.board.squares c) with
        | Success _ -> true
        | Failure _ -> false
        
    let doesTileHavePiece (st : state) (c : coord) : bool =
        match (doesTileExist st c) with
        | true ->
            match (st.lettersOnBoard.TryFind c) with
            | Some _ -> true
            | None   -> false
        | false -> false
        
    let getAdjacentPiece (st : state) (c : coord) =
        match st.lettersOnBoard.TryFind c with
        | Some (cVal, pVal) -> Some (cVal)
        | None -> None
        
    // 04. This functions takes a coordinate and checks its adjacent LEFT and UP tiles:
        
    let calcCoordUsingLength (length : int) (initCoord : coord) (dir : Direction) =
        let rec aux (n : int) (cAcc : coord) =
            match n with
            | 0 -> cAcc
            | _ -> aux (n - 1) (stepDir dir cAcc)
        aux length initCoord
        
    let wordExists (dict : Dict) (list : List<coord * (char * int)>) =
        let word = List.map (fun (_, (c, _)) -> c) list |> List.toArray |> System.String
        ScrabbleUtil.Dictionary.lookup word dict
        
    let getPieceAtCoord (pob : Map<coord, (char * int)>) (c : coord) = pob.TryFind c
    
    // 06. Here we are collecting the words
    let collectLongestWord (initCoord : coord) (initHand : MultiSet.MultiSet<uint32>) (dict : Dict) (piecesOnBoard : Map<coord, (char * int)>) =
        
        let rec collectLongestWordHelper (currCoord : coord) (currHand : MultiSet.MultiSet<uint32>) (currDict : Dict) = 
            
            let startingPoint =
                match (piecesOnBoard.TryFind currCoord) with
                | Some (c, _) -> [c]
                | None -> toList (multisetToChar currHand)
        
            startingPoint
        
        collectLongestWordHelper initCoord initHand dict
        
        
    // 07. Here we are utilizing the fact that the depth of a Trie node represents the length of a word
    //     After having accumulated a dict, we then use it to search it in conjunction with the current
    //     pieces on the board, to determine what we can write.
    let locateWordUsingDictStep (startCoord : coord) (dir : Direction) (piecesInHand : string) (hand : MultiSet<uint32>) (dict : Dict) (piecesOnBoard : Map<coord, (char * int)>) =
        let (wordLength, currDict) =
            List.fold (fun ((trieDepth : int), (d : Dict)) c ->
                    let stepNode = step c dict
                    
                    match stepNode with
                    | Some (_, trieNode) ->
                        (trieDepth + 1, trieNode)
                    | None ->
                        (trieDepth, d)
                
                ) (0, dict) ([for c in piecesInHand do c])
            
        // 
        let tailCoord = calcCoordUsingLength wordLength startCoord dir
         
        // collectLongestWord tailCoord hand piecesInHand currDict piecesOnBoard
        
    // Here we follow the the pattern below:
    //      # 1 ::
    //      # 2 ::
    
    let traverseToLocateWords (st : state) (initCoord : coord) (dir : Direction) =
        let rec traverseInDirection (piecesOnBoard : Map<coord, (char * int)>) (currCoord : coord) (currDir : Direction) (accWordList : List<coord * uint32 * char * int>) =
            let stepCoordinate = stepDir currDir currCoord 
            
            match (piecesOnBoard.TryFind stepCoordinate) with
            | Some (c, pVal) ->
                traverseInDirection piecesOnBoard stepCoordinate currDir ((collectTileInfo stepCoordinate c pVal) :: accWordList)
            | None ->
                accWordList
        
        match ((stepDir dir initCoord) |> getAdjacentPiece st) with
        | Some _ -> None        // Case #1 :: Nothing, so in our Map.fold, we keep going
        | None   ->             // Case #2 :: Something! So we now reverse and go forwards to locate the entire word
            // We can safely assume that from here we can traverse forwards...
            let piece    = Map.find initCoord st.lettersOnBoard
            let pieceC   = fst piece
            let pieceP   = snd piece
            let tileInfo = collectTileInfo initCoord pieceC pieceP
            
            Some (traverseInDirection st.lettersOnBoard initCoord dir [tileInfo])
            
    
    // 10. In this method, we ...
    //     The result is a word in the form of List<coord * uint32 * (char * int)>
    let gatherWordsOnBoard (st : state) (c : coord) (dir : Direction) =
        Map.fold (fun (accWordList : List<coord * uint32 * char * int> list) (coordinate : coord) (character : char) ->
                  match (traverseToLocateWords st c dir) with
                  | Some word ->            
                      
                  
                  )
    
    let gatherAllPlayableWords = failwith "fuck"
        // let checkUp   = (stepDir Up c)   |> doesTileExistAndHavePiece st
        // let checkLeft = (stepDir Left c) |> doesTileExistAndHavePiece st