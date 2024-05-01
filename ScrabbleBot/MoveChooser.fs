module internal ScrabbleBot.MoveChooser

    open MultiSet
    open ScrabbleUtil
    open ScrabbleUtil.Dictionary
    open StateMonad
    
    
    // HAVE TO USE (coord * uint32 * (char * int))
        
    /////////////////////////////////////////////KEEP IN MIND////////////////////////////////////////////////////////
    // A much better approach is to start from a square on the board and build a word by incrementally interleave
    // placing pieces from your hand and using ones that are already placed on the board.
    // The step function from Dictionary.fs from Assignment 4 is useful here.
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // 01. Types:
    type state = { // copy-&-pasta ... no good
        board          : Parser.board
        dict           : ScrabbleUtil.Dictionary.Dict
        playerNumber   : uint32                          // Essentially Player ID
        hand           : MultiSet.MultiSet<uint32>       // Current pieces on hand
        // Newly Added:
        numPlayers     : uint32                          // Total number of players in game
        playerTurn     : uint32                          // Marks the current players turn
        timeout        : uint32 option                   // Time taken
        lettersOnBoard : Map<coord, (char * int)>        // The letters currently on the board
    }
    
    type Direction =
        | North
        | East
        | South
        | West
    
    // TODO :: Consider adding diagonal???
    type TileSurroundings =
        bool * bool * bool * bool
    
    type coord = int * int
    
    // 03. Character to Point evaluation gotten from Wikipage on Scrabble:
    // Gotten from: https://en.wikipedia.org/wiki/Scrabble_letter_distributions#:~:text=English%2Dlanguage%20editions%20of%20Scrabble,%C3%974%2C%20G%20%C3%973
    let charPointValues (c : char) =
        match System.Char.ToUpper(c) with
        | 'A' | 'E' | 'I' | 'O' | 'U' | 'L' | 'N' | 'S' | 'T' | 'R' -> 1
        | 'D' | 'G' -> 2
        | 'B' | 'C' | 'M' | 'P' -> 3
        | 'F' | 'H' | 'V' | 'W' | 'Y' -> 4
        | 'K' -> 5
        | 'J' | 'X' -> 8
        | 'Q' | 'Z' -> 10
        | _ -> failwith "Character not recognized so no point is given"
            
    // 04. Convert uint32 (from MS) to char:
    let uint32ToChar (ui : uint32) = char(ui + 64u)
    
    // 05. Convert char to uint32 MS:
    let charToUint32 (c : char) =
        if (c = '?') then 0u
        else uint32(System.Char.ToUpper(c)) - 64u
        
    let calculateCharListValue (cList : List<coord * (char * int)>)
        
    let rec removeFirst pred lst =
        match lst with
        | h::t when pred h -> t
        | h::t -> h::removeFirst pred t
        | _ -> []
    
    // 06. Check if MS contains a char (so your hand):
    let checkMSForChar (c : char) =
        c |> charToUint32 |> MultiSet.contains
    
    // 07. Convert hand to char list:
    let handToCharList (hand : MultiSet<uint32>) =
        hand |> MultiSet.toList |> List.map uint32ToChar
    
    // 08. Determine whether the tile even exists - we are playing with different boards, so maybe not :O
    let isTileReal (st : state) (coordinate : coord) =
        match (st.board.squares coordinate) with
        | Success _ -> true
        | Failure _ -> false
    
    // 09. Determine whether or not a tile is empty:
    let isTileEmpty (st : state) (coordinate : coord) =
        match st.lettersOnBoard.TryFind coordinate with
        | Some _ -> false
        | None   -> true
        
    let isTileNotEmpty (st : state) (coordinate : coord) =
        not(isTileEmpty st coordinate)
        
    // 10. Check if its both real AND empty:
    let isTileRealAndEmpty (st : state) (coordinate : coord) =
        (isTileReal st coordinate) && (isTileEmpty st coordinate)
        
    // 11. Given a direction, we shift the coordinate in that direction by '1':
    let stepDir (dir : Direction) ((x, y) : coord) : coord =
        match dir with
        | North -> (x, y - 1)
        | East  -> (x + 1, y)
        | South -> (x, y + 1)
        | West  -> (x - 1, y)
    
    // 12. Check if a tiles surrounding "tiles" are part of the board or not:
    let checkSurroundingTilesAreReal (st : state) (coordinate : coord) : TileSurroundings =
        let N = isTileReal st (stepDir North coordinate)
        let E = isTileReal st (stepDir East coordinate)
        let S = isTileReal st (stepDir South coordinate)
        let W = isTileReal st (stepDir West coordinate)
        
        (N, E, S, W)
        
    // 13. Check if a tiles surrounding "tiles" are even real as well as empty or not:
    let checkIfTileHasNeighbours (st : state) (coordinate : coord) : TileSurroundings =
        let N = isTileRealAndEmpty st (stepDir North coordinate)
        let E = isTileRealAndEmpty st (stepDir East coordinate)
        let S = isTileRealAndEmpty st (stepDir South coordinate)
        let W = isTileRealAndEmpty st (stepDir West coordinate)
        
        (N, E, S, W)
    
    // 14. Append & Prepend function operations: TODO :: Make a single / multiple character pattern matching ???
    let append (cList : List<coord * (char * int)>) cAppend = cList @ [cAppend]
    let prepend (cList : List<coord * (char * int)>) cPrepend = [cPrepend] @ cList
    
    // 15. Check to see whether the board is entirely clean:
    let isBoardClean (lettersOnBoard : Map<coord, (char * int)>) : bool = lettersOnBoard.IsEmpty
        
    // 16. Determine whether or not this is a word:
    let isWord (dict : Dict) (list : List<coord * (char * int)>) : bool =
        let word = List.map (fun (_, (y, _)) -> y) list |> List.toArray |> System.String
        Dictionary.lookup word dict
   
    // 17. Calculate the Points:
    let calculatePoints (word : List<coord * (char * int)>) =
        List.fold (fun acc (_, (_, pVal)) -> acc + pVal ) 0 word
        
    // Use this to determine presence of wildcard case 
    let isWildCardTile (lettersOnBoard : Map<coord, tile>) (tileCoord : coord) : bool =
        match (lettersOnBoard.TryFind(tileCoord)) with
        | Some x when x.Count > 1 -> true
        | Some _ -> false
        | None   -> failwith "Coordinate was invalid"
        
        
    // 18. 
    let collectWordInDir (st : state) (coordinate : coord) (word : List<coord * (char * int)>) (dir : Direction) : List<coord * (char * int)> =
        let rec move (currCoord : coord) (accWord : List<coord * (char * int)>) =
            let newTileCoordinate =
                match dir with
                // !#! Vertical
                | North -> stepDir North currCoord
                | South -> stepDir South currCoord
                // !#! Horizontal
                | West -> stepDir West currCoord
                | East -> stepDir East currCoord
                
            if not(isTileRealAndEmpty st newTileCoordinate) then
                let targetTile = Map.find newTileCoordinate st.lettersOnBoard
                
                (match dir with
                | North | East -> append accWord (newTileCoordinate, targetTile)
                | South | West -> prepend accWord (newTileCoordinate, targetTile)) |> move newTileCoordinate
                
            else
                accWord
        
        move coordinate word
    
    // 19.
    let checkVertical (st : state) (coordinate : coord) (word : List<coord * (char * int)>) =
        // TODO :: True-False check against North-South
        
        match (checkIfTileHasNeighbours st coordinate) with
        | true,  _, true,  _ ->
            let first  = collectWordInDir st coordinate word North 
            let second = collectWordInDir st coordinate word South
                        
            isWord st.dict (first.[0..first.Length - 2] @ second)
        | true,  _, false, _ ->
            isWord st.dict (collectWordInDir st coordinate word North)
        | false, _, true,  _ ->
            isWord st.dict (collectWordInDir st coordinate word South)
        | false, _, false, _ -> false
            
    // 20. 
    let checkHorizontal (st : state) (coordinate : coord) (word : List<coord * (char * int)>) =
        match (checkIfTileHasNeighbours st coordinate) with
        | _, true,  _,  true ->
            let first  = collectWordInDir st coordinate word West 
            let second = collectWordInDir st coordinate word East
                        
            isWord st.dict (first.[0..first.Length - 2] @ second)
        | _, true,  _, false ->
            isWord st.dict (collectWordInDir st coordinate word West)
        | _, false, _,  true ->
            isWord st.dict (collectWordInDir st coordinate word East)
        | _, false, _, false -> false
    
    ////////////////////////////////////////////////////////////////////////////////////////////////
    //                                      ###IDEA###
    //  # The idea is to maintain a Map of all the letters that have been placed on the board and each time
    //    it is our bots turn, we then go over the board, checking the 'lettersOnBoard' Map for any relevant
    //    letters which we may use to construct our play.
    //  # 
    //  # X & Y coordinates:
    //      -> Negative X is LEFT       (WEST)
    //      -> Positive X is RIGHT      (EAST)
    //      -> Negative Y is TOP        (NORTH)
    //      -> Positive Y is BOTTOM     (SOUTH)
    //  
    //  # We use the type (coord * uint32 * (char * int)), when working with lists of words 
    
    
    // 01. Look at all the words on board. Recursively move in any direction that is NOT empty, building the words as you go
    // 02. Once we find a complete world, either "append" or "prepend" letters from our hand 
    // 03. Then for each letter we "append" or "prepend" to a word, we first check its a word
    // 04. Finally, we check that ALL surrounding tiles are not impacted, if they are, go to the next word - otherwise we add it and are done.
            
    // let locateWordOnBoard (initialCoord : coord) ()
    
    let locateWord (st : state) (coordAdjuster : coord -> coord) joinFunc (checker) (initCoord : coord) startingPoint =
        let rec locateWordHelper (hand) (word : List<coord * (char * int)>) (remainingWord : List<coord * (char * int)>) (currCoord : coord) gatheredWords =
            hand |> List.fold
                (fun x k ->
                    let charr = Map.find k st.lettersOnBoard
                    let newWord = joinFunc word (currCoord, k, charr)
                    let newExisting = joinFunc remainingWord (currCoord, k, charr)
                    
                    if (not (checker st currCoord [(currCoord, (k, charr))])) then
                        gatheredWords
                    else if isWord st.dict newExisting then
                        Map.fold (fun acc k v -> Map.add k v acc) (locateWordHelper (removeFirst (fun m -> m = k ) hand) newWord newExisting (coordAdjuster currCoord) (Map.add (calculateCharListValue newWord) newWord gatheredWords)) x
                    else
                        Map.fold (fun acc k v -> Map.add k v acc) (locateWordHelper (removeFirst (fun m -> m = k ) hand) newWord newExisting (coordAdjuster currCoord) gatheredWords) x
                ) gatheredWords
                
        locateWordHelper initCoord (MultiSet.toList st.hand) [] startingPoint Map.empty
    
    
    
    
    let updatePlacedTiles (moveList) (lettersOnBoard : Map<coord, (char * int)>) =
        List.fold (fun acc (c, v) -> Map.add (coord(c)))
        
    
    // https://www.fssnip.net/4t/title/Permutations
    let rec permutations (A : 'a list) =
        if List.isEmpty A then [[]] else
        [
            for a in A do
            yield! A |> List.filter (fun x -> x <> a) 
                     |> permutations
                     |> List.map (fun xs -> a::xs)
        ]
        
    
    // TODO: If hand.empty then +50 points
        
    
    let playMove (st : state) =
        let handAsCharList = handToCharList
        
        printf "Size of hand: %i" (MultiSet.size st.hand)
        
        printf "Get hand as characters: %A" handAsCharList
        
        if isBoardClean st.lettersOnBoard then do
            printf "Board is clean --- You make the first play"
            
            