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
    | Center
    
    // 03. Compass type - [intention was to make a neighbors algorithm to deal with incorrect placement]
    type Compass = | North | East | South | West | NorthWest | NorthEast | SouthEast | SouthWest
    
    // 04. Determine whether or not a tile has neighbors
    // Order is :: N, E, S, W, NE, SE, NW, SW
    type Neighbours = (bool * bool * bool * bool * bool * bool * bool * bool)
    
    // 05. Addition or subtract type used for indication:
    type AddOrSub =
    | Add
    | Sub

    // 06. Basic 'dirToCoord':
    let dirToCoord (dir : Direction) =
        match dir with
        | Horizontal -> (1, 0)
        | Vertical   -> (0, 1)
        | Center     -> (0, 0)
        
    // 07. Enhanced 'dirToCoord' [Not utilized]:
    let diagToCoord (diag : Compass) =
        match diag with
        | North     -> (0, -1)
        | East      -> (1,  0)
        | South     -> (0,  1)
        | West      -> (-1, 0)
        | NorthWest -> (-1,-1)
        | NorthEast -> (1, -1)
        | SouthEast -> (1,  1)
        | SouthWest -> (-1, 1)
    
    // 08. Join 2 coordinates together:
    let assimilateCoords ((x1, y1) : Coordinate) ((x2, y2) : Coordinate) (isAdd : AddOrSub) : Coordinate =
        match isAdd with
        | Add  -> x2 + x1, y2 + y1
        | Sub  -> x2 - x1, y2 - y1
        
    // 09. Syntax sugar for addition:
    let (.+.) (c1 : Coordinate) (c2 : Coordinate) : Coordinate =
        assimilateCoords c1 c2 Add
    
    
    // 10. Simply a "syntactical-sugar" operator for building strings - gotten from here:
    // https://www.fssnip.net/by/title/Building-Strings
    let (++) (s : StringBuilder) (c : char) : StringBuilder =
        s.Append c
        
    // 11. Represents a word along with its starting coordinate and direction it moves in:
    type WordPackage  = (string * (Coordinate * Direction))
    
    // 12. Convert a character to its uint32 identifier as requested by the server when parsing to a move:
    let charToUint32 (c : char) = uint32(System.Char.ToUpper(c)) - 64u
    
    // 13. Convert a unint32 to its respective char:
    let uint32ToChar (uint : uint32) = char(uint + 64u)
    
    // 14. Multiset to char
    let msToChar = map (fun msType -> uint32ToChar msType)
    
    // 15. Check if tile exists:
    let doesTileExist (st : state) (c : Coordinate) =
        match (st.board.squares c) with
        | Success _ -> true
        | Failure _ -> false
        
    // 16. Check if tile has a piece:
    let doesTileHavePiece (st : state) (c : Coordinate) : bool =
        match (doesTileExist st c) with
        | true ->
            match (st.piecesOnBoard.TryFind c) with
            | Some _ -> true
            | None   -> false
        | false -> false
    
    // 17. Syntactical sugar for checking if tile has a piece:
    let (./.) (st : state) (coordinate : Coordinate) =
        doesTileHavePiece st coordinate
    
    // 18. Check all neighbors:
    // N, E, S, W, NE, SE, NW, SW
    let checkAllNeighbours (st : state) (coordinate : Coordinate) : Neighbours =
        let N =   coordinate .+. diagToCoord North
        let E =   coordinate .+. diagToCoord East
        let S =   coordinate .+. diagToCoord South
        let W =   coordinate .+. diagToCoord West
        let NE =  coordinate .+. diagToCoord NorthEast
        let NW =  coordinate .+. diagToCoord NorthWest
        let SE =  coordinate .+. diagToCoord SouthEast
        let SW =  coordinate .+. diagToCoord SouthWest
        
        ((./.) st N, (./.) st E, (./.) st S, (./.) st W, (./.) st NE, (./.) st SE, (./.) st NW, (./.) st SW)
        
    // 19. Check if board is empty:
    let isBoardEmpty (st : state) =
        (st.piecesOnBoard.Count = 0)
    
    // 20. For the length of the word, find its position in coordinates.
    //     We are utilizing the fact that words may ONLY go from LEFT-to-RIGHT and UP-to-DOWN
    let calcCoordUsingLength (length : int) (initCoord : Coordinate) (dir : Direction) =
        let rec aux (n : int) (cAcc : Coordinate) =
            match n with
            | 0 -> cAcc
            | _ -> aux (n - 1) (assimilateCoords cAcc (dirToCoord dir) Add)
        aux length initCoord
        
    // 21. Confirm with our trie that its actually word:
    let isWord (st : state) (word : string) =
        ScrabbleUtil.Dictionary.lookup word st.dict
        
    // 22. Get the longest word in a specified direction
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
                    
                    // Make sure we know which pieces have been used and which are left in our hand:
                    let piecesLeftInHand =
                        match (piecesOnBoard.TryFind coordinate) with
                        | Some _ -> hand
                        | None   -> removeSingle (charToUint32 character) hand  // As there is no piece on the board, take it from our hand
                    
                    let newCoordinate = (assimilateCoords coordinate (dirToCoord dir) Add)
                    
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
        
        
    // 23. Construct a dict trie of a specific word (in the arguments 'wordAcc') to then see if we can add any more to it
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
        
    // 24. Used in conjunction with M.25 to locate a string word in some given direction:
    let traverseToLocateWords (st : state) (initCoord : Coordinate) (initDir : Direction) : string option =
        let rec traverseInDirection (piecesOnBoard : Map<coord, (char * int)>) (currCoord : Coordinate) (currDir : Direction) (accWord : StringBuilder) =
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
            
    // 25. In this method, we construct our list of words, going in a particular direction and assimilating all
    //     the strings - this will include all playable words. We will later then filter out which are actually
    //     playable and which are not:
    let assimilatePieces (st : state) (direction : Direction) =
        Map.fold (
            fun (accWordList : (string * (Coordinate * Direction)) list) (coordinate : Coordinate) ((character : char), _) ->
                match (traverseToLocateWords st coordinate direction) with
                | Some locatedWord ->
                    (locatedWord, (coordinate, direction)) :: accWordList
                | None ->
                    accWordList
                
            ) [] st.piecesOnBoard
        
    // 26. Along with functions '24' and '25', we traverse in the core directions playable directions
    //     'horizontally' or 'vertically' - to try and find playable words:
    let gatherWordsOnTheBoard (st : state) =
        let wordsGoingUpToDown    = assimilatePieces st Direction.Horizontal
        let wordsGoingLeftToRight = assimilatePieces st Direction.Vertical
                
        wordsGoingUpToDown @ wordsGoingLeftToRight
        
    // 27. Collect all the possible words we can play based on what we have on the board:
    // TODO :: Currently under construction
    let collectAllTheWordsWeCanPlay (st : state) : (string * (coord * Direction)) list =                
        let gatheredWordsCurrentlyOnBoard = gatherWordsOnTheBoard st
        
        let rec aux (locatedWordsOnBoard : (string * (coord * Direction)) list) (acc : (string * (coord * Direction)) list) =
            match locatedWordsOnBoard with
            | [] -> acc
            | x::xs ->
                let s = (fst x)
                let (c, d) = (snd x)
                let locatedWord = constructDictTrie st s c d 
                
                let accumulatedRes =
                    if locatedWord.Length > 0 then
                        (locatedWord, (c, d)) :: acc
                    else acc
                                        
                aux xs accumulatedRes 
            
        aux gatheredWordsCurrentlyOnBoard []
        
        
    
        
    // 28. We filter through to find the longest playable word, and from this collectively go through all the
    //     previous methods, performing the necessary word gathering, tile checking, etc...
    //     This method needs an additional safe guard to make sure it takes into consideration a playable word.
    //     Currently, we discard a word in replacement for a longer "better" word, but this ruins our chances to
    //     actually play a word we are allowed to put down:
    // let getLongestWord (st : state) =
    //     let rec getLongestWordHelper (acc : (string * (Coordinate * Direction))) (listOfPlayableWords : (string * (Coordinate * Direction)) list) =
    //         match listOfPlayableWords with
    //         | []    -> acc
    //         | x::xs ->
    //             let (currS, (_, _)) = x
    //             let (accS, (_, _)) = acc
    //             
    //             let acc' = if String.length currS > String.length accS then x else acc
    //             getLongestWordHelper acc' xs
    //     
    //     getLongestWordHelper ("", ((0,0), Center)) (collectAllTheWordsWeCanPlay st)
           
    
    
    // 29. Our play is on the first turn.
    //     In this case, the board is clean and a play must be made in the center of the board.
    //     The safest and easiest play is just to simply go along the horizontal axis and place
    //     the words starting from the center:
    let longestWordWeCanPlayOnTurnOne (st : state) =
        locateLongestWordInADirection st "" st.dict (dirToCoord Center) Horizontal
    
    // 30. Here we parse the string into a list of moves the server can be given to perform our play.
    //     The coord and Direction indicate where we start and where we are heading:
    let parseBotMove (st : state) ((s, (c, d)) : string * (Coordinate * Direction)) : ((int * int) * (uint32 * (char * int))) list =
        let rec parseBotMoveHelper (commandAcc : ((int * int) * (uint32 * (char * int))) list) (cList : char list) (coordinate : Coordinate) (direction : Direction) =
            match cList with
            | []    -> List.rev commandAcc
            | x::xs -> 
                // COMMAND ORDER :: 0 0 1A1 0 1
                let charID               = charToUint32 x
                let charValue            = charPointValues x
                let (currX, currY)       = coordinate
                let (xDirCoor, yDirCoor) = (dirToCoord direction)
                let (newXCoor, newYCoor) = assimilateCoords (xDirCoor, yDirCoor) (currX, currY) Add
                let command              = ((currX, currY), (charID, (x, charValue)))
                
                match (doesTileHavePiece st (currX, currY)) with
                | true ->   // Tile is already on the board, so don't add it. 
                    parseBotMoveHelper commandAcc xs (newXCoor, newYCoor) direction
                | false ->  // Tile is NOT already on the board, so do add it.
                    parseBotMoveHelper (command :: commandAcc) xs (newXCoor, newYCoor) direction
                
        parseBotMoveHelper [] ([for c' in s do c']) c d
        
    // TODO :: Change to style below - me like more...
    let createTempState (st:state) (moves:list<coord * (uint32 * (char * int))>) =
        List.fold (fun acc move ->
            let (coord, (_,(char, charPoints))) = move
            let newPlayedLetters = acc.piecesOnBoard |> Map.add coord (char, charPoints)
            let st' =
                {
                    st with
                        piecesOnBoard = newPlayedLetters
                }
            st'
        ) st moves
    
    
    let createTempState2 (st : state) (plannedPlay : (Coordinate * (uint32 * (char * int))) list) : state =
        let rec aux (acc : state) (moves : (Coordinate * (uint32 * (char * int))) list) : state =
            match moves with
            | [] -> acc
            | (coor, (_, (cVal, pVal)))::xs ->
                
                let st' =
                    {
                        st with
                            piecesOnBoard = st.piecesOnBoard |> Map.add coor (cVal, pVal)
                    }
                aux st' xs
                
        aux st plannedPlay
    
    // 
    let listOfAllWordsWeCanPlay (st : state) =
        List.fold (fun (acc: (string * (Coordinate * Direction)) list) (key,value) ->
            let (coord, dir) = value
            let longestWord = constructDictTrie st key coord dir
            if longestWord.Length > 0 then
                (longestWord, value)::acc
            else
                acc
        ) [] (gatherWordsOnTheBoard st)
    
    //
    let longestWordWeCanPlay (st : state) =
        List.fold (fun (accWord : (string * (Coordinate * Direction))) (currWord : string * (Coordinate * Direction)) ->
                let move = parseBotMove st currWord
                let tempSt = createTempState st move
                let tempBoard = gatherWordsOnTheBoard tempSt
                
                let isWordValid =
                    List.fold (fun (sVal : bool) (s : string, (c, _)) ->
                            match sVal with
                            | true ->
                                if s.Length = 1    then true
                                elif (isWord st s) then true
                                else                    false 
                            | false -> false    
                        ) true tempBoard
                    
                // If coordinate is outside the playable range, handle that here...    
                
                if isWordValid && (fst currWord).Length > (fst accWord).Length then
                    currWord
                else
                    accWord
                
        ) ("", ((0,0), Center)) (listOfAllWordsWeCanPlay st)
    
    // 31. Print statement to double check the parsed syntax when debugging:
    let printParseMove (parsedMove : ((int * int) * (uint32 * (char * int))) list) =
        parsedMove |> List.iter (printf "Command :: %A\n")
    
    let printNumPiecesInHand (st : state) =
        printf "\nNum pieces in hand: %i\n" (MultiSet.size st.hand) 
    
    let printNumPiecesOnBoard (st : state) =
        printf "\nNum pieces on board: %i\n" st.piecesOnBoard.Count 
        
    let printNumPiecesLeft (st : state) =
        printf "\nPieces Left: %i\n" st.piecesLeft
    
    let printAllWordsWeCouldPlay (lstOfWords : (string * (coord * Direction)) list) =
        printf "\n!================== ALL WORDS ==================!\n"
        lstOfWords |> List.iter (fun (s, (c, d)) ->
                printf "String     :: %s\n" s
                printf "Coordinate :: (%i,%i)\n" (fst c) (snd c)
                printf "Direction  :: %A\n" d
            )
        printf "\n!===============================================!\n"