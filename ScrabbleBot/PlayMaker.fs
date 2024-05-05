module internal PlayMaker

    open GameState
    open StateMonad
    open ScrabbleUtil
    open ScrabbleUtil.Dictionary
    open MultiSet
    open System.Text
    
    // 00. Character to Point evaluation built and based entirely on the Wikipedia page on Scrabble:
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
        
        
    // 01. Coordinate on the board:
    type Coordinate = int * int
    
    // 02. Direction going in X / Y direction:
    type Direction =
    | Horizontal
    | Vertical
    
    type AddOrSub =
    | Add
    | Sub

    let dirToCoord (dir : Direction) =
        match dir with
        | Horizontal -> (1,  0)
        | Vertical   -> (0,  1)
        
    let assimilateCoords ((x1, y1) : Coordinate) ((x2, y2) : Coordinate) (isAdd : AddOrSub) : Coordinate =
        match isAdd with
        | Add  -> x2 + x1, y2 + y1
        | Sub -> x2 - x1, y2 - y1
    
    // 03. Simply a "syntactical-sugar" operator for building strings - gotten from here:
    // https://www.fssnip.net/by/title/Building-Strings
    let (++) (s : StringBuilder) (c : char) : StringBuilder =
        s.Append c
        
    // 03. Represents a word along with its starting coordinate and direction it moves in
    type WordPackage  = (string * (Coordinate * Direction))
    
    // 04. ...
    let charToUint32 (c : char) = uint32(System.Char.ToUpper(c)) - 64u
    
    // 05. ...uint32ToChar
    let uint32ToChar (uint : uint32) = char(uint + 64u)
    
    let msToChar = map (fun msType -> uint32ToChar msType)
    
    // 06. ...
    type PieceInfo  = (Coordinate * (uint32 * (char * int)))
    
    // 07. ...
    let doesTileExist (st : state) (c : Coordinate) =
        match (st.board.squares c) with
        | Success _ -> true
        | Failure _ -> false
        
    // 08. ... 
    let doesTileHavePiece (st : state) (c : Coordinate) : bool =
        match (doesTileExist st c) with
        | true ->
            match (st.piecesOnBoard.TryFind c) with
            | Some _ -> true
            | None   -> false
        | false -> false
    
    
    // 09. For the length of the word, find its position in coordinates.
    //     We are utilizing the fact that words may ONLY go from LEFT-to-RIGHT and UP-to-DOWN
    let calcCoordUsingLength (length : int) (initCoord : Coordinate) (dir : Direction) =
        let rec aux (n : int) (cAcc : Coordinate) =
            match n with
            | 0 -> cAcc
            | _ -> aux (n - 1) (assimilateCoords cAcc (dirToCoord dir) Add)
        aux length initCoord
        
        
    // 10. ...
    let wordExists (dict : Dict) (lst : (coord * char * int) list) =
        let word = List.map (fun (_, c, _) -> c) lst |> List.toArray |> System.String
        ScrabbleUtil.Dictionary.lookup word dict
        
    // 11. Get the longest word in a specified direction
    let locateLongestWordInADirection (st : GameState.state) (initWord : string) (initDict : Dict) (initCoord : coord) (initDir : Direction) =
        let rec locateLongestWordHelper (wordAcc : string) (hand : MultiSet<uint32>) (dict : Dict) (piecesOnBoard : Map<coord, (char * int)>) (coordinate : coord) (dir : Direction) = 
            
            let startingPoint =
                match (piecesOnBoard.TryFind coordinate) with
                | Some (c, i) -> MultiSet.toList (MultiSet.addSingle (c) MultiSet.empty)
                | None        -> MultiSet.toList (msToChar hand)
                
            List.fold (fun (currLongestWord : string) (character : char) ->
                
                let currWord = (wordAcc + character.ToString())
                
                match (step character dict) with
                | Some (isWord, trieNode) ->
                    
                    // We have to keep track of the pieces we have used:
                    let piecesLeftInHand =
                        match (piecesOnBoard.TryFind coordinate) with
                        | Some _ -> hand
                        | None   -> (MultiSet.removeSingle (charToUint32 character) hand)
                    
                    let (x1, y1) = coordinate
                    let (x2, y2) = (dirToCoord dir)
                    
                    let newCoordinate = x2 + x1, y2 + y1
                    
                    let anchorWord = locateLongestWordHelper currWord piecesLeftInHand trieNode piecesOnBoard newCoordinate dir
                    
                    if isWord && currWord.Length > currLongestWord.Length && doesTileExist st newCoordinate then 
                        currWord
                    elif anchorWord.Length > currLongestWord.Length then
                        anchorWord
                    else
                        currLongestWord
                    
                | None  -> currLongestWord
                ) "" startingPoint
            
        locateLongestWordHelper initWord st.hand initDict st.piecesOnBoard initCoord initDir
        
        
    // 12. Construct a dict trie of a specific word (in the arguments 'wordAcc') to then see if we can add any more to it
    let constructDictTrie (st : state) (wordAcc : string) (startCoord : coord) (direction : Direction) : string =
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
        
    // 13. 
    let traverseToLocateWords (st : state) (initCoord : coord) (initDir : Direction) : string option =
        let rec traverseInDirection (piecesOnBoard : Map<coord, (char * int)>) (currCoord : coord) (currDir : Direction) (accWord : StringBuilder) =
            // We step in the specified direction to locate the necessary word...
            let stepCoordinate = (assimilateCoords (dirToCoord currDir) currCoord Add)
            
            match (piecesOnBoard.TryFind stepCoordinate) with
            | Some (c, _) ->
                traverseInDirection piecesOnBoard stepCoordinate currDir (accWord ++ c)
            | None ->
                accWord.ToString()
        
        
        let stepCoordinate = (assimilateCoords (dirToCoord initDir) initCoord Sub)
        
        match (st.piecesOnBoard.TryFind stepCoordinate) with
        | Some _ -> None       // Case #1 :: Nothing, so in our Map.fold, we keep going
        | None   ->           // Case #2 :: Something! So we now reverse and go forwards to locate the entire word
            let (cVal, _) = Map.find initCoord st.piecesOnBoard
                        
            Some (traverseInDirection st.piecesOnBoard initCoord initDir (StringBuilder() ++ cVal))
            
    // 14. In this method, 
    let gatherWordsOnBoard (st : state) (direction : Direction) =
        Map.fold (
            fun (accWordList : (string * (coord * Direction)) list) (coordinate : coord) ((character : char), _) ->
                match (traverseToLocateWords st coordinate direction) with
                | Some locatedWord ->
                    (locatedWord, (coordinate, direction)) :: accWordList
                | None ->
                    accWordList
                
            ) [] st.piecesOnBoard
        
    // 15. Along with functions '13' and '14', we traverse in the necessary directions to try and locate 
    let gatherPotentialWords (st : state) =
        let wordsGoingUpToDown    = gatherWordsOnBoard st Direction.Horizontal
        let wordsGoingLeftToRight = gatherWordsOnBoard st Direction.Vertical
                
        wordsGoingUpToDown @ wordsGoingLeftToRight
        
        
    // 16 
    let parseBotMove (st : state) ((s, (c, d)) : string * (coord * Direction)) : ((int * int) * (uint32 * (char * int))) list =
        let rec parseBotMoveHelper (commandAcc : ((int * int) * (uint32 * (char * int))) list) (piecePos : int) (cList : char list) (coordinate : coord) (direction : Direction) =
            match cList with
            | []    -> commandAcc
            | x::xs -> 
                // 0 0 1A1 0 1 ...etc...
                let charID               = charToUint32 x
                let charValue            = charPointValues x
                let (xDirCoor, yDirCoor) = (dirToCoord direction)
                let (newXCoor, newYCoor) = assimilateCoords ((xDirCoor * piecePos, yDirCoor * piecePos)) coordinate Add
                let command              = ((newXCoor, newYCoor), (charID, (x, charValue)))
                
                match (doesTileHavePiece st (newXCoor, newYCoor)) with
                | true ->   // Tile is already on the board, so don't add it. 
                    parseBotMoveHelper commandAcc (piecePos + 1) xs (newXCoor, newYCoor) direction
                | false ->  // Tile is NOT already on the board, so do add it.
                    parseBotMoveHelper (command :: commandAcc) (piecePos + 1) xs (newXCoor, newYCoor) direction
                
        parseBotMoveHelper [] 0 ([for c' in s do c']) c d