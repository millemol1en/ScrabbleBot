namespace ScrabBoyz

open ScrabbleBot
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open ScrabbleBot.MoveChooser
open MoveChooser

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
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

    let mkState (_board : Parser.board) (_dict : ScrabbleUtil.Dictionary.Dict) (_playerNum : uint32) (_hand : MultiSet.MultiSet<uint32>) (_numPlayers : uint32) (_playerTurn : uint32) (_timeout : uint32 option) (_lettersOnBoard : Map<coord, (char * int)>)  =
        {
            board = _board
            dict = _dict
            playerNumber = _playerNum  
            hand = _hand
            numPlayers = _numPlayers
            playerTurn = _playerTurn 
            timeout = _timeout
            lettersOnBoard = _lettersOnBoard
        }

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand

module Scrabble =
    open System.Threading
    open State 
    //////////////////////////////////////////NEW FUNCTIONS//////////
    
    let switchTurn (st : State.state) = if st.playerTurn = st.numPlayers then uint32 1 else (st.playerTurn + uint32 1)

    let updateMoveIntoState (moveList : list<coord * (uint32 * (char * int))>) (st : state) =
        List.fold (fun stAcc move ->
            let (coord, (_,(char, charPoints))) = move
            debugPrint (sprintf "Inserting move %A %A\n" coord (char))
            // We add 
            let newPlayedLetters = stAcc.lettersOnBoard |> Map.add coord (char, charPoints)
            mkState stAcc.board stAcc.dict stAcc.playerNumber stAcc.hand stAcc.numPlayers st.playerTurn stAcc.timeout newPlayedLetters
        ) st moveList
        
    let updateHand moveList st newPieces =
        // Get the indices of the tiles we've just played
        let playedIndexes = 
            moveList 
            |> Seq.map (fun move -> 
                let (_, (charuint, (_, _))) = move
                charuint
                ) 
            |> Seq.toList 
            |> MultiSet.ofList

        // Filter out the tiles we've just played:
        let subtractedHand = MultiSet.subtract (hand st) playedIndexes

        // Add new ones coming in from the server:
        List.fold (fun acc (indexOfLetter, letterCount) -> 
        MultiSet.add indexOfLetter letterCount acc) subtractedHand newPieces
    ///////////
        
    let playGame cstream (pieces : Map<uint32, tile>) (st : state) =
        let rec aux (st : state) (isMyTurn : bool) =
            // TODO :: Start timeout here
            
            debugPrint(sprintf "CURRENT PLAYER: %d \n" (st.playerTurn))
            debugPrint(sprintf "IS MY TURN? %b \n" isMyTurn)
            debugPrint(sprintf "NUM PIECES ON BOARD %d \n" st.lettersOnBoard.Count)
            
            if isMyTurn then
                debugPrint("\n=======================\n**** My Turn ****\n=======================\n")
                Print.printHand pieces (State.hand st)
                
                let input =  System.Console.ReadLine()
                let move  = RegEx.parseMove input
                
                // debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move)  //| keep the debug lines. They are useful.
                
                if move.Length = 0 then
                    send cstream (SMPass)
                else 
                    send cstream (SMPlay move)                                              //| TODO :: This is used to send our move to the server (p. 19)
                
            else
                debugPrint("\n=======================\n**** OPPONENT TURN ****\n=======================\n")

            //////////// Perform MoveChooser //////////////
            let msg = recv cstream                                                          //| TODO :: This is used to receive our opponents move from the server (p. 19)
            
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->                
                let insertMovesIntoState = updateMoveIntoState ms st
                let newHand = updateHand ms st newPieces
            
                let st' = mkState st.board st.dict st.playerNumber newHand st.numPlayers (switchTurn st) st.timeout insertMovesIntoState.lettersOnBoard
                aux st' (st.playerNumber % st.numPlayers + 1u = st.playerNumber)    // TODO Package into function
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                
                let insertMovesIntoState = updateMoveIntoState ms st
                
                let st' = mkState st.board st.dict st.playerNumber st.hand st.numPlayers (switchTurn st) st.timeout insertMovesIntoState.lettersOnBoard
                aux st' (pid % st.numPlayers + 1u = st.playerNumber)
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st
                aux st' (st.playerNumber % st.numPlayers + 1u = st.playerNumber)
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st false

        // Kickoff
        if (st.playerTurn = st.playerNumber) then
            aux st true  
        else
            aux st false 

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict)           
            (numPlayers : uint32)                       //| Number of players present
            (playerNumber : uint32)                     //| Player ID
            (playerTurn  : uint32)                      //| Who's turn is it anyways?
            (hand : (uint32 * uint32) list)             //| Starting hand
            (tiles : Map<uint32, tile>)                 //| Tile lookup table
            (timeout : uint32 option)   
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet numPlayers playerTurn timeout Map.empty)
        