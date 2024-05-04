module internal PlayMaker

    open System.Collections.Generic
    open System.Text
    open GameState
    open MultiSet
    open ScrabbleUtil.Dictionary
    open StateMonad
    open Parser
    open ScrabbleUtil.DebugPrint
    
    // 00. Character to Point evaluation gotten from Wikipage on Scrabble:
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
        // | '#' -> 0                   @!Wildcard case!@
    
    
    // 01. Type declaration:
    type coord = int * int
    
    type cList = List<coord * uint32 * (char * int)>
    
    type Direction =
        | Up
        | Down
        | Left
        | Right
        
    type wordPackage = (string * (coord * Direction))
        
    let getOppDirection (dir : Direction) =
        match dir with
        | Up    -> Down
        | Left  -> Right
        | Right -> Left
        | Down  -> Up
    
    // 02. As MultiSets define our hand using the uint32 identifier, we use the following method to convert between characters and their unique ID
    //     Theory for this is given here - [https://stackoverflow.com/questions/29638419/whats-the-theory-behind-subtracting-from-toupper]
    let charToMultiSetID (c : char) = uint32(System.Char.ToUpper(c)) - 64u
    
    let multiSetIdToChar (msID : uint32) = char(msID + 64u)
    
    let multisetToChar = MultiSet.map (fun msID -> multiSetIdToChar msID)
    
    let collectTileInfo (pieceCoordinate : coord) (pieceChar : char) (piecePoint : int) : (coord * uint32 * char * int) =
        (pieceCoordinate, (charToMultiSetID pieceChar), pieceChar, piecePoint)
    
    // 03. 
    // https://www.fssnip.net/by/title/Building-Strings
    let (++) (s : StringBuilder) (c : char) : StringBuilder =
        s.Append c
        
    let (.++.) (s : StringBuilder) (c : string) : StringBuilder =
        s.Append c
        
    let completeString (s : StringBuilder) : string = s.ToString()
        
    // 03. 
    let stepDir (dir : Direction) ((x, y) : coord) : coord =
        match dir with
        | Up -> (x, y + 1)
        | Right  -> (x + 1, y)
        | Down -> (x, y - 1)
        | Left  -> (x - 1, y)
        
    let doesTileExist (st : state) (c : coord) =
        match (st.board.squares c) with
        | Success _ -> true
        | Failure _ -> false
        
    let doesTileHavePiece (st : state) (c : coord) : bool =
        match (doesTileExist st c) with
        | true ->
            match (st.piecesOnBoard.TryFind c) with
            | Some _ -> true
            | None   -> false
        | false -> false
        
    let getAdjacentPiece (st : state) (c : coord) =
        match st.piecesOnBoard.TryFind c with
        | Some (cVal, pVal) -> Some (cVal)
        | None -> None
        
        
    // 04. This functions takes a coordinate and checks its adjacent LEFT and UP tiles:
        
    let calcCoordUsingLength (length : int) (initCoord : coord) (dir : Direction) =
        let rec aux (n : int) (cAcc : coord) =
            match n with
            | 0 -> cAcc
            | _ -> aux (n - 1) (stepDir dir cAcc)
        aux length initCoord
        
    let wordExists (dict : Dict) (lst : (coord * char * int) list) =
        let word = List.map (fun (_, c, _) -> c) lst |> List.toArray |> System.String
        ScrabbleUtil.Dictionary.lookup word dict
        
    let getPieceAtCoord (pob : Map<coord, (char * int)>) (c : coord) = pob.TryFind c
    
    // 06. Here we are collecting the words
    //
    // 
    let locateLongestWordInADirection (st : GameState.state) (initWord : string) (initDict : Dict) (initCoord : coord) (initDir : Direction) =
        let rec locateLongestWordHelper (wordAcc : string) (hand : MultiSet<uint32>) (dict : Dict) (piecesOnBoard : Map<coord, (char * int)>) (coordinate : coord) (dir : Direction) = 
            
            let startingPoint =
                match (piecesOnBoard.TryFind coordinate) with
                | Some (c, i) -> MultiSet.toList (MultiSet.addSingle (c) MultiSet.empty)
                | None        -> MultiSet.toList (multisetToChar hand)
                
            List.fold (fun (currLongestWord : string) (character : char) ->
                
                let currWord = (wordAcc + character.ToString())
                
                match (step character dict) with
                | Some (isWord, trieNode) ->
                    
                    let newHand =
                        match (piecesOnBoard.TryFind coordinate) with
                        | Some _ -> hand
                        | None   -> (MultiSet.removeSingle (charToMultiSetID character) hand)
                    
                    let newCoordinate = stepDir dir coordinate
                    
                    let anchorWord = locateLongestWordHelper currWord newHand trieNode piecesOnBoard newCoordinate dir
                    
                    if isWord && currWord.Length > currLongestWord.Length && doesTileExist st newCoordinate then 
                        currWord
                    elif anchorWord.Length > currLongestWord.Length then
                        anchorWord
                    else
                        currLongestWord
                    
                | None  -> currLongestWord
                ) "" startingPoint
            
        locateLongestWordHelper initWord st.hand initDict st.piecesOnBoard initCoord initDir
    
    // 07. "..."
    //     Here we are utilizing the fact that the depth of a Trie node represents the length of a word
    //     After having accumulated a dict, we then use it to search it in conjunction with the current
    //     pieces on the board, to determine what we can write.
    
    let locateWordUsingDictStep (st : state) (wordAcc : string) (startCoord : coord) (direction : Direction) : string =
        let (wordLength, currDict) =
            List.fold (fun ((trieDepth : int), (d : Dict)) c ->
                    let stepNode = step c st.dict
                    
                    match stepNode with
                    | Some (_, trieNode) ->
                        (trieDepth + 1, trieNode)
                    | None ->
                        (trieDepth, d)
                
                ) (0, st.dict) ([for c in wordAcc do c])
            
        locateLongestWordInADirection st wordAcc currDict (calcCoordUsingLength wordLength startCoord direction) direction
        
    // let locateLongestWordOnFirstTurn (st : state) =
    //     
        
        
    // Here we follow the the pattern below:
    //      # 1 ::
    //      # 2 ::
    
    let traverseToLocateWords (st : state) (initCoord : coord) (initDir : Direction) : string option =
        let rec traverseInDirection (piecesOnBoard : Map<coord, (char * int)>) (currCoord : coord) (currDir : Direction) (accWord : StringBuilder) =
            let stepCoordinate = stepDir currDir currCoord 
            
            match (piecesOnBoard.TryFind stepCoordinate) with
            | Some (c, _) ->
                traverseInDirection piecesOnBoard stepCoordinate currDir (accWord ++ c)
            | None ->
                (completeString accWord)
        
        
        match ((stepDir initDir initCoord) |> getAdjacentPiece st) with
        | Some _ -> None        // Case #1 :: Nothing, so in our Map.fold, we keep going
        | None   ->           // Case #2 :: Something! So we now reverse and go forwards to locate the entire word
            let (cVal, _) = Map.find initCoord st.piecesOnBoard
            
            let oppDirection = getOppDirection initDir
            
            Some (traverseInDirection st.piecesOnBoard initCoord oppDirection (StringBuilder() ++ cVal))
            
    
    // 10. In this method, we ...
    //     
    //     The 'acc' type is:
    //          # 1 :: 'string' -> this is the actual word we have built
    //          # 2 :: 'coord'  -> the starting coordinate of the word
    //          # 3 :: 'dir'    -> the direction the word goes - either LEFT-to-RIGHT or UP-to-DOWN
    let gatherWordsOnBoard (st : state) (direction : Direction) =
        Map.fold (
            fun (accWordList : (string * (coord * Direction)) list) (coordinate : coord) ((character : char), _) ->
                match (traverseToLocateWords st coordinate direction) with
                | Some locatedWord ->
                    (locatedWord, (coordinate, direction)) :: accWordList
                | None ->
                    accWordList
                
            ) [] st.piecesOnBoard
    
    
    // TODO :: This needs to be fixed
    //         Currently adding 1 too many letters
    //         And it is goofy as hell
    let gatherPotentialWords (st : state) =
        
        // TODO :: This works like ass and needs fixing...
        let wordsGoingUpToDown    = gatherWordsOnBoard st Direction.Up
        let wordsGoingLeftToRight = gatherWordsOnBoard st Direction.Right
        
        // forcePrint(sprintf "\n\n====================\n  wordsGoingUpToDown    :: %A\n  wordsGoingLeftToRight :: %A \n====================\n\n" wordsGoingUpToDown wordsGoingLeftToRight)
        
        wordsGoingUpToDown @ wordsGoingLeftToRight
        
        
    // getLongestWord
    
       
    // 12. We finally get to the portion of the program where we parse a move into
    //     
    //
    //     The direction in which we move is specified by the 3*tuple value 'Direction'
    let parseBotMove (st : state) ((s, (c, d)) : string * (coord * Direction)) : ((int * int) * (uint32 * (char * int))) list =
        let rec parseBotMoveHelper (commandAcc : ((int * int) * (uint32 * (char * int))) list) (cList : char list) (coordinate : coord) (direction : Direction) =
            match cList with
            | []    -> commandAcc
            | x::xs -> 
                // 0 0 1A1 0 1 ...etc...
                let charID         = charToMultiSetID x
                let charValue      = charPointValues x
                let (xCoor, yCoor) = coordinate
                let newCoord       = stepDir direction coordinate
                let command        = ((xCoor, yCoor), (charID, (x, charValue)))
                
                match (doesTileHavePiece st newCoord) with
                | true ->   // Tile is already on the board, so don't add it. 
                    parseBotMoveHelper commandAcc xs newCoord direction
                | false ->  // Tile is NOT already on the board, so do add it.
                    parseBotMoveHelper (command :: commandAcc) xs newCoord direction
                
        parseBotMoveHelper [] ([for c' in s do c']) c d
                
                
                
                
                    