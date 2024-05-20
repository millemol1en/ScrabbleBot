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
    
    // 03. Addition or subtract type used for indication:
    type AddOrSub =
    | Add
    | Sub

    // 04. Basic 'dirToCoord':
    let dirToCoord (dir : Direction) =
        match dir with
        | Horizontal -> (1, 0)
        | Vertical   -> (0, 1)
        | Center     -> (0, 0)
        
    // 05. Join 2 coordinates together:
    let assimilateCoords ((x1, y1) : Coordinate) ((x2, y2) : Coordinate) (isAdd : AddOrSub) : Coordinate =
        match isAdd with
        | Add  -> x2 + x1, y2 + y1
        | Sub  -> x2 - x1, y2 - y1
        
    // 06. Syntax sugar for addition:
    let (.+.) (c1 : Coordinate) (c2 : Coordinate) : Coordinate =
        assimilateCoords c1 c2 Add
    
    
    // 07. Simply a "syntactical-sugar" operator for building strings - gotten from here:
    // https://www.fssnip.net/by/title/Building-Strings
    let (++) (s : StringBuilder) (c : char) : StringBuilder =
        s.Append c
        
    // 08. Represents a word along with its starting coordinate and direction it moves in:
    type WordPackage  = (string * (Coordinate * Direction))
    
    // 09. Convert a character to its uint32 identifier as requested by the server when parsing to a move:
    let charToUint32 (c : char) = uint32(System.Char.ToUpper(c)) - 64u
    
    // 10. Convert a unint32 to its respective char:
    let uint32ToChar (uint : uint32) = char(uint + 64u)
    
    // 11. Multiset to char
    let msToChar = map (fun msType -> uint32ToChar msType)
    
    // 12. Check if tile exists:
    let doesTileExist (st : state) (c : Coordinate) =
        match (st.board.squares c) with
        | Success _ -> true
        | Failure _ -> false
        
    // 13. Check if tile has a piece:
    let doesTileHavePiece (st : state) (c : Coordinate) : bool =
        match (doesTileExist st c) with
        | true ->
            match (st.piecesOnBoard.TryFind c) with
            | Some _ -> true
            | None   -> false
        | false -> false
    
    // 14. Check if board is empty:
    let isBoardEmpty (st : state) =
        (st.piecesOnBoard.Count = 0)
    
    // 15. For the length of the word, find its position in coordinates.
    //     We are utilizing the fact that words may ONLY go from LEFT-to-RIGHT and UP-to-DOWN
    let calcCoordUsingLength (length : int) (initCoord : Coordinate) (dir : Direction) =
        let rec aux (n : int) (cAcc : Coordinate) =
            match n with
            | 0 -> cAcc
            | _ -> aux (n - 1) (assimilateCoords cAcc (dirToCoord dir) Add)
        aux length initCoord
        
    // 16. Confirm with our trie that its actually word:
    let isWord (st : state) (word : string) =
        ScrabbleUtil.Dictionary.lookup word st.dict
        
    // 17. Get the longest word in a specified direction
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
                    
                    // We need to further traverse any anchor words which may be associated with the current coordinate and direction.
                    // 
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
        
        
    // 18. Construct a dict trie of a specific word (in the arguments 'wordAcc') to then see if we can add any more to it
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
        
    // 19. Used in conjunction with M.25 to locate a string word in some given direction:
    let traverseToLocateWordsAsync (st : state) (initCoord : Coordinate) (initDir : Direction) : Async<string option> = async {
        let rec traverseInDirection (piecesOnBoard : Map<coord, (char * int)>) (currCoord : Coordinate) (currDir : Direction) (accWord : StringBuilder) = async {
            let stepCoordinate = (assimilateCoords (dirToCoord currDir) currCoord Add)
            match (piecesOnBoard.TryFind stepCoordinate) with
            | Some (c, _) ->
                let! word = traverseInDirection piecesOnBoard stepCoordinate currDir (accWord ++ c)
                return word
            | None ->
                return accWord.ToString()
        }
        
        let stepCoordinate = (assimilateCoords (dirToCoord initDir) initCoord Sub)
        match (st.piecesOnBoard.TryFind stepCoordinate) with
        | Some _ -> return None
        | None ->
            let (cVal, _) = Map.find initCoord st.piecesOnBoard
            let! word = traverseInDirection st.piecesOnBoard initCoord initDir (StringBuilder() ++ cVal)
            return Some word
    }
            
    // 20. In this method, we construct our list of words, going in a particular direction and assimilating all
    //     the strings - this will include all playable words. We will later then filter out which are actually
    //     playable and which are not:
    let assimilatePiecesAsync (st : state) (direction : Direction) : Async<(string * (Coordinate * Direction)) list> = async {
        let tasks = 
            st.piecesOnBoard
            |> Map.toSeq
            |> Seq.map (fun (coordinate, (_: char, _)) -> async {
                match! traverseToLocateWordsAsync st coordinate direction with
                | Some locatedWord -> return Some (locatedWord, (coordinate, direction))
                | None -> return None
            })
        
        let! results = tasks |> Async.Parallel
        return results |> Array.choose id |> List.ofArray
    }
        
    // 21. Along with functions '24' and '25', we traverse in the core directions playable directions
    //     'horizontally' or 'vertically' - to try and find playable words:
    let gatherWordsOnTheBoardAsync (st : state) : Async<(string * (Coordinate * Direction)) list> = async {
        let! wordsGoingUpToDown = assimilatePiecesAsync st Direction.Horizontal
        let! wordsGoingLeftToRight = assimilatePiecesAsync st Direction.Vertical
        
        return wordsGoingUpToDown @ wordsGoingLeftToRight
    }
    
    // 22. Our play is on the first turn.
    //     In this case, the board is clean and a play must be made in the center of the board.
    //     The safest and easiest play is just to simply go along the horizontal axis and place
    //     the words starting from the center:
    let longestWordWeCanPlayOnTurnOne (st : state) =
        locateLongestWordInADirection st "" st.dict (dirToCoord Center) Horizontal
    
    // 23. Here we parse the string into a list of moves the server can be given to perform our play.
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
        
    // 24: Create a temporary state. This is then later used by traversing through it to confirm that all the newly placed
    //     tiles make up words. 
    let createTempState (st: state) (plannedPlay: (Coordinate * (uint32 * (char * int))) list) =
        let rec loop (accState : state) (remainingMoves : (Coordinate * (uint32 * (char * int))) list) =
            match remainingMoves with
            | [] -> accState
            | (coord, (_, (char, charPoints))) :: tail ->
                let newPlayedLetters = accState.piecesOnBoard |> Map.add coord (char, charPoints)
                
                let st' =
                    {
                        accState with
                            piecesOnBoard = newPlayedLetters
                    }
                
                loop st' tail
        loop st plannedPlay
    
    // 25: It is 'might' play because we are not sure yet whether these words are accepted on the board
    //     and therefore, accepted by the server.
    //     We use this function below in the proceeding functions to confirm whether or not the word is playable.
    let listOfAllWordsWeMightPlayAsync (st : state) : Async<(string * (Coordinate * Direction)) list> = async {
        let! wordsOnBoard = gatherWordsOnTheBoardAsync st
        
        return List.fold (fun (acc: (string * (Coordinate * Direction)) list) (key, value) ->
            let (coord, dir) = value
            let longestWord = constructDictTrie st key coord dir
            if longestWord.Length > 0 then
                (longestWord, value) :: acc
            else
                acc
        ) [] wordsOnBoard
    }
    
    // 26: Confirm that a word is valid 
    let isWordValidAsync (st : state) (tempBoard : (string * (Coordinate * Direction)) list) : Async<bool> = async {
        let sVal =
             List.fold (fun (sVal: bool) (s: string, (c, d)) ->
                 match sVal with
                 | true ->
                     if s.Length = 1 then true
                     elif (isWord st s) then true
                     // To play on all board consider ::
                     //     -> Recursively go over all tiles to check they exist, specifically use this idea of determining
                     //        the length with the follow:
                     //
                     //                             " c + (s.Length * (dirToCoord d)) "
                     //
                     //     -> This can be more simply calculated using the function "calcCoordUsingLength()" which is above
                     //     -> Thereafter, we can use the 'doesTileExist' to check that the word would be valid on the
                     //        specific board we are playing on.
                     
                     else false
                 | false -> false
             ) true tempBoard
        return sVal
    }
    
    // 27. This function finds all the playable words and from it, locates the longest one.
    //     On further consideration, it would be better to double check the 'tile', a discriminated union
    //     type provided to us, and thereby from that calculate which play would be highest value.
    let longestPlayableWordAsync (st : state) : Async<(string * (Coordinate * Direction))> = async {
        let! allWords = listOfAllWordsWeMightPlayAsync st
        
        return List.fold (fun (accWord : (string * (Coordinate * Direction))) (currWord : string * (Coordinate * Direction)) ->
            async {
                let  move       = parseBotMove st currWord
                let  tempSt     = createTempState st move
                let! tempBoard  = gatherWordsOnTheBoardAsync tempSt
                
                // Run another asynchronous value to check whether this is
                let! isWordValid = isWordValidAsync st tempBoard
                
                if isWordValid && (fst currWord).Length > (fst accWord).Length then
                    return currWord
                else
                    return accWord
            } |> Async.RunSynchronously
        ) ("", ((0, 0), Center)) allWords
    }
    
    //////////////////////////////////////////////////
    //                                              //
    //                                              //
    //         FUNCTIONS FOR PRINTING INFO          //
    //                                              //
    //                                              //
    //////////////////////////////////////////////////
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
        
        
    //////////////////////////////////////////////////
    //                                              //
    //                                              //
    //              DEPRECIATED CODE                //
    //                                              //
    //                                              //
    //////////////////////////////////////////////////
    type Compass = | North | East | South | West | NorthWest | NorthEast | SouthEast | SouthWest

    type Neighbours = (bool * bool * bool * bool * bool * bool * bool * bool)
    
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
    
    let (./.) (st : state) (coordinate : Coordinate) =
        doesTileHavePiece st coordinate
        
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