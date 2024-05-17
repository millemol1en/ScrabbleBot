module internal PlayMaker

open GameState
open StateMonad
open ScrabbleUtil
open ScrabbleUtil.Dictionary
open MultiSet
open System.Text

// 00. Character to Point evaluation built and based entirely on the Wikipedia page on Scrabble:
// Gotten from: https://en.wikipedia.org/wiki/Scrabble_letter_distributions#:~:text=English%2Dlanguage%20editions%20of%20Scrabble,%C3%974%2C%20G%20%C3%973
let charPointValues (c: char) =
    match System.Char.ToUpper(c) with
    | 'A'
    | 'E'
    | 'I'
    | 'O'
    | 'U'
    | 'L'
    | 'N'
    | 'S'
    | 'T'
    | 'R' -> 1
    | 'D'
    | 'G' -> 2
    | 'B'
    | 'C'
    | 'M'
    | 'P' -> 3
    | 'F'
    | 'H'
    | 'V'
    | 'W'
    | 'Y' -> 4
    | 'K' -> 5
    | 'J'
    | 'X' -> 8
    | 'Q'
    | 'Z' -> 10
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
type Compass =
    | North
    | East
    | South
    | West
    | NorthWest
    | NorthEast
    | SouthEast
    | SouthWest

// 04. Determine whether or not a tile has neighbors
// Order is :: N, E, S, W, NE, SE, NW, SW
type Neighbours = (bool * bool * bool * bool * bool * bool * bool * bool)

// 05. Addition or subtract type used for indication:
type AddOrSub =
    | Add
    | Sub

// 06. Basic 'dirToCoord':
let dirToCoord (dir: Direction) =
    match dir with
    | Horizontal -> (1, 0)
    | Vertical -> (0, 1)
    | Center -> (0, 0)

// 07. Enhanced 'dirToCoord' [Not utilized]:
let diagToCoord (diag: Compass) =
    match diag with
    | North -> (0, -1)
    | East -> (1, 0)
    | South -> (0, 1)
    | West -> (-1, 0)
    | NorthWest -> (-1, -1)
    | NorthEast -> (1, -1)
    | SouthEast -> (1, 1)
    | SouthWest -> (-1, 1)

// 08. Join 2 coordinates together:
let assimilateCoords ((x1, y1): Coordinate) ((x2, y2): Coordinate) (isAdd: AddOrSub) : Coordinate =
    match isAdd with
    | Add -> x2 + x1, y2 + y1
    | Sub -> x2 - x1, y2 - y1

// 09. Syntax sugar for addition:
let (.+.) (c1: Coordinate) (c2: Coordinate) : Coordinate = assimilateCoords c1 c2 Add


// 10. Simply a "syntactical-sugar" operator for building strings - gotten from here:
// https://www.fssnip.net/by/title/Building-Strings
let (++) (s: StringBuilder) (c: char) : StringBuilder = s.Append c

// 11. Represents a word along with its starting coordinate and direction it moves in:
type WordPackage = (string * (Coordinate * Direction))

// 12. Convert a character to its uint32 identifier as requested by the server when parsing to a move:
let charToUint32 (c: char) = uint32 (System.Char.ToUpper(c)) - 64u

// 13. Convert a unint32 to its respective char:
let uint32ToChar (uint: uint32) = char (uint + 64u)

// 14. Multiset to char
let msToChar = map (fun msType -> uint32ToChar msType)

// 15. Check if tile exists:
let doesTileExist (st: state) (c: Coordinate) =
    match (st.board.squares c) with
    | Success _ -> true
    | Failure _ -> false

// 16. Check if tile has a piece:
let doesTileHavePiece (st: state) (c: Coordinate) : bool =
    match (doesTileExist st c) with
    | true ->
        match (st.piecesOnBoard.TryFind c) with
        | Some _ -> true
        | None -> false
    | false -> false

// 17. Syntactical sugar for checking if tile has a piece:
let (./.) (st: state) (coordinate: Coordinate) = doesTileHavePiece st coordinate

// 18. Check all neighbors:
// N, E, S, W, NE, SE, NW, SW
let checkAllNeighbours (st: state) (coordinate: Coordinate) : Neighbours =
    let N = coordinate .+. diagToCoord North
    let E = coordinate .+. diagToCoord East
    let S = coordinate .+. diagToCoord South
    let W = coordinate .+. diagToCoord West
    let NE = coordinate .+. diagToCoord NorthEast
    let NW = coordinate .+. diagToCoord NorthWest
    let SE = coordinate .+. diagToCoord SouthEast
    let SW = coordinate .+. diagToCoord SouthWest

    ((./.) st N, (./.) st E, (./.) st S, (./.) st W, (./.) st NE, (./.) st SE, (./.) st NW, (./.) st SW)

let countNumNeighbours (neighbours: Neighbours) : int =
    let (n, e, s, w, ne, se, nw, sw) = neighbours
    [ n; e; s; w; ne; se; nw; sw ] |> List.sumBy (fun b -> if b then 1 else 0)

//
let checkHorizontalDirNeighbors (st: state) (coordinate: Coordinate) : (bool * bool) =
    let NE = assimilateCoords (diagToCoord NorthEast) coordinate Add
    let SE = assimilateCoords (diagToCoord SouthEast) coordinate Add

    ((doesTileHavePiece st NE), (doesTileHavePiece st SE))

//
let checkVerticalDirNeighbors (st: state) (coordinate: Coordinate) : (bool * bool) =
    let SE = assimilateCoords (diagToCoord SouthEast) coordinate Add
    let SW = assimilateCoords (diagToCoord SouthWest) coordinate Add

    ((doesTileHavePiece st SW), (doesTileHavePiece st SE))



// 19. Check if board is empty:
let isBoardEmpty (st: state) = (st.piecesOnBoard.Count = 0)

// 20. For the length of the word, find its position in coordinates.
//     We are utilizing the fact that words may ONLY go from LEFT-to-RIGHT and UP-to-DOWN
let calcCoordUsingLength (length: int) (initCoord: Coordinate) (dir: Direction) =
    let rec aux (n: int) (cAcc: Coordinate) =
        match n with
        | 0 -> cAcc
        | _ -> aux (n - 1) (assimilateCoords cAcc (dirToCoord dir) Add)

    aux length initCoord


// 21. Confirm with our trie that its actually word:
let isWord (st: state) (word: string) =
    ScrabbleUtil.Dictionary.lookup word st.dict
// Refactored generateAdjacentWords function
let generateAdjacentWords st (x, y) character dir =
    // Define the opposite direction for building words in the opposite direction
    let oppositeDirection =
        function
        | North -> South
        | East -> West
        | South -> North
        | West -> East
        | _ -> failwith "Invalid direction for opposite"

    // Build a word from a starting coordinate in a specified direction
    let buildWordFromCoord coord direction =
        let rec build acc currCoord =
            match st.piecesOnBoard.TryFind(currCoord) with
            | Some(c, _) -> build (c.ToString() + acc) (assimilateCoords currCoord (diagToCoord direction) Add)
            | None -> acc

        build "" coord

    // Get the adjacent coordinates based on the direction of the main word
    let adjCoords =
        match dir with
        | Horizontal -> [ (x, y) .+. diagToCoord North; (x, y) .+. diagToCoord South ]
        | Vertical -> [ (x, y) .+. diagToCoord East; (x, y) .+. diagToCoord West ]
        | _ -> failwith "Invalid main direction"

    // Generate words from adjacent coordinates
    let adjWords =
        adjCoords
        |> List.collect (fun coord ->
            let dir1, dir2 =
                match dir with
                | Horizontal -> North, South
                | Vertical -> East, West
                | _ -> failwith "Invalid main direction"

            let word1 =
                buildWordFromCoord (assimilateCoords coord (diagToCoord (oppositeDirection dir1)) Add) dir1

            let word2 = buildWordFromCoord (assimilateCoords coord (diagToCoord dir2) Add) dir2
            [ word1; word2 ])
        |> List.filter (fun word -> word.Length > 1) // Filter out single characters
    // Filter out words that are not in the dictionary
    List.filter (isWord st) adjWords

// Helper function to check if a tile is empty
let isTileEmpty st (x, y) =
    match st.piecesOnBoard.TryFind(x, y) with
    | Some _ -> false
    | None -> true

// Helper function to check if the move is within the board boundaries
let isWithinBoundaries (x, y) dir wordLength =
    let (dx, dy) = dirToCoord dir
    let endX = x + (dx * (wordLength - 1))
    let endY = y + (dy * (wordLength - 1))
    // Assuming the board is 15x15
    endX >= 0 && endX < 15 && endY >= 0 && endY < 15

// Helper function to check if the word is adjacent to existing words
let isAdjacentToExistingWord st (x, y) dir (word: string) =
    let directions = [ North; East; South; West ]

    let wordCoords =
        [ for i in 0 .. word.Length - 1 -> (x + i * fst (dirToCoord dir), y + i * snd (dirToCoord dir)) ]

    let isAdjacent coord =
        directions
        |> List.exists (fun compassDir ->
            let (dx, dy) = diagToCoord compassDir
            let adjacentCoord = (fst coord + dx, snd coord + dy)
            st.piecesOnBoard.ContainsKey(adjacentCoord))

    wordCoords |> List.exists isAdjacent


// Modified isWordValidOnBoard function with additional checks
let isWordValidOnBoard st word (x, y) dir =
    let rec helper word (x, y) dir =
        match word with
        | "" -> true // End of the word reached, no more letters to check
        | _ ->
            match st.piecesOnBoard.TryFind(x, y) with
            | Some(c, _) when c = word.[0] ->
                // The tile contains the expected character, continue with the rest of the word
                helper word.[1..] ((x, y) .+. dirToCoord dir) dir
            | None ->
                // The tile is empty, check if it's within boundaries and adjacent to existing words
                let withinBoundaries = isWithinBoundaries (x, y) dir word.Length
                let adjacentToExistingWord = isAdjacentToExistingWord st (x, y) dir
                // Debugging: Log the status of boundary and adjacency checks
                printfn "Within boundaries? %b" withinBoundaries
                printfn "Adjacent to existing word? %b" (adjacentToExistingWord word)
                withinBoundaries && adjacentToExistingWord word
            | _ -> false // The tile is not valid for placement

    helper word (x, y) dir


// 22. Get the longest word in a specified direction
let locateLongestWordInADirection
    (st: GameState.state)
    (initWord: string)
    (initDict: Dict)
    (initCoord: coord)
    (initDir: Direction)
    =
    let rec locateLongestWordHelper
        (wordAcc: string)
        (hand: MultiSet<uint32>)
        (dict: Dict)
        (piecesOnBoard: Map<coord, (char * int)>)
        (coordinate: coord)
        (dir: Direction)
        =

        let startingPoint =
            match (piecesOnBoard.TryFind coordinate) with
            | Some(c, i) -> MultiSet.toList (MultiSet.addSingle (c) MultiSet.empty)
            | None -> MultiSet.toList (msToChar hand)

        List.fold
            (fun (currLongestWord: string) (character: char) ->

                let currWord = (wordAcc + character.ToString())

                match (step character dict) with
                | Some(isWord, trieNode) ->

                    // Make sure we know which pieces have been used and which are left in our hand:
                    let piecesLeftInHand =
                        match (piecesOnBoard.TryFind coordinate) with
                        | Some _ -> hand
                        | None -> (MultiSet.removeSingle (charToUint32 character) hand)

                    let newCoordinate = (assimilateCoords coordinate (dirToCoord dir) Add)

                    let isPlacementValid =
                        if doesTileExist st newCoordinate then
                            match (piecesOnBoard.TryFind newCoordinate) with
                            | Some(c, _) -> c = character // The tile contains the same character
                            | None ->
                                // The tile is empty, check if the placement forms a valid word
                                let adjWords = generateAdjacentWords st newCoordinate character dir

                                List.forall (fun word -> isWordValidOnBoard st word newCoordinate dir) adjWords
                        else
                            false // The tile is outside the board


                    let anchorWord =
                        locateLongestWordHelper currWord piecesLeftInHand trieNode piecesOnBoard newCoordinate dir

                    if
                        isWord
                        && currWord.Length > currLongestWord.Length
                        && doesTileExist st newCoordinate
                        && isPlacementValid
                    then
                        currWord
                    elif anchorWord.Length > currLongestWord.Length then
                        anchorWord
                    else
                        currLongestWord

                | None -> currLongestWord)
            ""
            startingPoint

    locateLongestWordHelper initWord st.hand initDict st.piecesOnBoard initCoord initDir

// 23. Construct a dict trie of a specific word (in the arguments 'wordAcc') to then see if we can add any more to it
let constructDictTrie (st: state) (wordAcc: string) (startCoord: coord) (direction: Direction) : string =
    let (wordLength, currDict) =
        List.fold
            (fun ((trieDepth: int), (d: Dict)) c ->
                let stepNode = step c st.dict

                match stepNode with
                | Some(_, trieNode) -> (trieDepth + 1, trieNode)
                | None -> (trieDepth, d)

            )
            (0, st.dict)
            ([ for c in wordAcc do
                   c ])


    locateLongestWordInADirection st wordAcc currDict (calcCoordUsingLength wordLength startCoord direction) direction




// 24. Used in conjunction with M.25 to locate a string word in some given direction:
let traverseToLocateWords (st: state) (initCoord: coord) (initDir: Direction) : string option =
    let rec traverseInDirection
        (piecesOnBoard: Map<coord, (char * int)>)
        (currCoord: coord)
        (currDir: Direction)
        (accWord: StringBuilder)
        =
        // We step in the specified direction to locate the necessary word...
        let stepCoordinate = (assimilateCoords (dirToCoord currDir) currCoord Add)

        match (piecesOnBoard.TryFind stepCoordinate) with
        | Some(c, _) -> traverseInDirection piecesOnBoard stepCoordinate currDir (accWord ++ c)
        | None -> accWord.ToString()


    let stepCoordinate = (assimilateCoords (dirToCoord initDir) initCoord Sub)

    match (st.piecesOnBoard.TryFind stepCoordinate) with
    | Some _ -> None // Case #1 :: Nothing, so in our Map.fold, we keep going
    | None -> // Case #2 :: Something! So we now reverse and go forwards to locate the entire word
        let (cVal, _) = Map.find initCoord st.piecesOnBoard

        Some(traverseInDirection st.piecesOnBoard initCoord initDir (StringBuilder() ++ cVal))

// 25. In this method, we construct our list of words, going in a particular direction and assimilating all
//     the strings - this will include all playable words. We will later then filter out which are actually
//     playable and which are not:
let assimilatePieces (st: state) (direction: Direction) =
    Map.fold
        (fun (accWordList: (string * (coord * Direction)) list) (coordinate: coord) ((character: char), _) ->
            match (traverseToLocateWords st coordinate direction) with
            | Some locatedWord -> (locatedWord, (coordinate, direction)) :: accWordList
            | None -> accWordList

        )
        []
        st.piecesOnBoard

// 26. Along with functions '24' and '25', we traverse in the core directions playable directions
//     'horizontally' or 'vertically' - to try and find playable words:
let gatherWordsOnTheBoard (st: state) =
    let wordsGoingUpToDown = assimilatePieces st Direction.Horizontal
    let wordsGoingLeftToRight = assimilatePieces st Direction.Vertical

    wordsGoingUpToDown @ wordsGoingLeftToRight

// 27. Collect all the possible words we can play based on what we have on the board:
let collectAllTheWordsWeCanPlay (st: state) : (string * (coord * Direction)) list =
    let gatheredWordsCurrentlyOnBoard = gatherWordsOnTheBoard st

    let rec wordsBotMightPlayHelper
        (locatedWordsOnBoard: (string * (coord * Direction)) list)
        (acc: (string * (coord * Direction)) list)
        =
        match locatedWordsOnBoard with
        | [] -> acc
        | x :: xs ->
            let s = (fst x)
            let (c, d) = (snd x)
            let locatedWord = constructDictTrie st s c d

            // Debugging: Print the word and its validation status
            printfn "Checking word: %s at %A direction %A" locatedWord c d
            printfn "Is word in dictionary? %b" (isWord st locatedWord)
            printfn "Is word valid on board? %b" (isWordValidOnBoard st locatedWord c d)

            let accumulatedRes =
                if
                    locatedWord.Length > 0
                    && (isWord st locatedWord)
                    && isWordValidOnBoard st locatedWord c d
                then
                    printfn "Word added: %s" locatedWord
                    (locatedWord, (c, d)) :: acc
                else
                    printfn "Word rejected: %s" locatedWord
                    acc

            wordsBotMightPlayHelper xs accumulatedRes

    wordsBotMightPlayHelper gatheredWordsCurrentlyOnBoard []



// 28. We filter through to find the longest playable word, and from this collectively go through all the
//     previous methods, performing the necessary word gathering, tile checking, etc...
//     This method needs an additional safe guard to make sure it takes into consideration a playable word.
//     Currently, we discard a word in replacement for a longer "better" word, but this ruins our chances to
//     actually play a word we are allowed to put down:
let getLongestWord (st: state) =
    let rec getLongestWordHelper
        (acc: (string * (coord * Direction)))
        (listOfPlayableWords: (string * (coord * Direction)) list)
        =
        match listOfPlayableWords with
        | [] -> acc
        | x :: xs ->
            let (currS, (_, _)) = x
            let (accS, (_, _)) = acc

            let acc' = if String.length currS > String.length accS then x else acc
            getLongestWordHelper acc' xs

    getLongestWordHelper ("", ((0, 0), Center)) (collectAllTheWordsWeCanPlay st)

let test (st: state) =
    let rec getLongestWordHelper
        (acc: (string * (coord * Direction)))
        (listOfPlayableWords: (string * (coord * Direction)) list)
        =
        match listOfPlayableWords with
        | [] -> acc
        | x :: xs ->
            let (currS, (currCoor, _)) = x
            let (accS, (accCoor, _)) = acc

            // If we place horizontally, we need to consider NE and SE
            // If we place vertically, we need to consider SE and SW

            let currPieceNumNeighbors = countNumNeighbours (checkAllNeighbours st currCoor)
            let accPieceNumNeighbors = countNumNeighbours (checkAllNeighbours st accCoor)

            // Print the current word and number of neighbors
            // printfn "Current word: %s, neighbors: %d" currS currPieceNumNeighbors

            // let acc' =
            //     if currPieceNumNeighbors > accPieceNumNeighbors then
            //         x
            //     else
            //         acc

            let acc' = if currS.Length > accS.Length then x else acc

            getLongestWordHelper acc' xs

    // Get all playable words and print the number of words
    let allWords = collectAllTheWordsWeCanPlay st
    printfn "Number of playable words: %d" (List.length allWords)

    getLongestWordHelper ("", ((0, 0), Center)) allWords



// 29. Our play is on the first turn.
//     In this case, the board is clean and a play must be made in the center of the board.
//     The safest and easiest play is just to simply go along the horizontal axis and place
//     the words starting from the center:
let longestWordWeCanPlayOnTurnOne (st: state) =
    locateLongestWordInADirection st "" st.dict (dirToCoord Center) Vertical

// 30. Here we parse the string into a list of moves the server can be given to perform our play.
//     The coord and Direction indicate where we start and where we are heading:
let parseBotMove
    (st: state)
    ((s, (c, d)): string * (coord * Direction))
    : ((int * int) * (uint32 * (char * int))) list =
    let rec parseBotMoveHelper
        (commandAcc: ((int * int) * (uint32 * (char * int))) list)
        (cList: char list)
        (coordinate: coord)
        (direction: Direction)
        =
        match cList with
        | [] -> List.rev commandAcc
        | x :: xs ->
            // COMMAND ORDER :: 0 0 1A1 0 1
            let charID = charToUint32 x
            let charValue = charPointValues x
            let (currX, currY) = coordinate
            let (xDirCoor, yDirCoor) = (dirToCoord direction)
            let (newXCoor, newYCoor) = assimilateCoords (xDirCoor, yDirCoor) (currX, currY) Add
            let command = ((currX, currY), (charID, (x, charValue)))

            match (doesTileHavePiece st (currX, currY)) with
            | true -> // Tile is already on the board, so don't add it.
                parseBotMoveHelper commandAcc xs (newXCoor, newYCoor) direction
            | false -> // Tile is NOT already on the board, so do add it.
                parseBotMoveHelper (command :: commandAcc) xs (newXCoor, newYCoor) direction

    parseBotMoveHelper
        []
        ([ for c' in s do
               c' ])
        c
        d


// 31. Print statement to double check the parsed syntax when debugging:
let printParseMove (parsedMove: ((int * int) * (uint32 * (char * int))) list) =
    parsedMove |> List.iter (printf "Command :: %A\n")


let printAllWordsWeCouldPlay (lstOfWords: (string * (coord * Direction)) list) =
    printf "\n!================== ALL WORDS ==================!\n"

    lstOfWords
    |> List.iter (fun (s, (c, d)) ->
        printf "String     :: %s\n" s
        printf "Coordinate :: (%i,%i)\n" (fst c) (snd c)
        printf "Direction  :: %A\n" d)

    printf "\n!===============================================!\n"

let isVowel (c: char) =
    match c with
    | 'A'
    | 'E'
    | 'I'
    | 'O'
    | 'U' -> true
    | _ -> false

let vowelCount (st: state) =
    let handAsChar = toList (msToChar st.hand)

    List.fold
        (fun acc letter ->
            match isVowel letter with
            | true -> acc + 1
            | false -> acc)
        0
        handAsChar
