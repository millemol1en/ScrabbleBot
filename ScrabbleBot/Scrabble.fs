namespace ScrabBoyz

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

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
    open GameState
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    let mkState b d pn h pob = {
        GameState.board = b
        GameState.dict = d
        GameState.playerNumber = pn
        GameState.hand = h
        GameState.piecesOnBoard = pob;
    }
    
    // We take the responded moves from the server and place them into our 'piecesOnBoard' map
    let insertMovesIntoState (ms : (coord * (uint32 * (char * int))) list) (pob : Map<coord, (char * int)>) =
        let rec aux (moves : (coord * (uint32 * (char * int))) list) (acc : Map<coord, (char * int)>) =
            match moves with
            | [] -> acc
            | (coor, (_, (cVal, pVal)))::xs ->
                aux xs (acc |> Map.add coor (cVal, pVal))
                
        aux ms pob
    
    
    
module Scrabble =
    open System.Threading
    open State
    open GameState
    
    
    let isBoardEmpty (st : GameState.state) =
        (st.piecesOnBoard.Count = 0)
    
    let playGame cstream pieces (st : GameState.state) =
        let rec aux (st : GameState.state) =
            Print.printHand pieces (st.hand)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            // forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            let input =  System.Console.ReadLine()
            let move = RegEx.parseMove input

            if (isBoardEmpty st) then
                forcePrint ("The board is empty\n") 
            else 
                forcePrint (sprintf "The board currently has %i many pieces\n" st.piecesOnBoard.Count) 

            let allWordsOnTheBoard = (PlayMaker.gatherPotentialWords st)
            
            // TODO :: Move this OUT!!!
            // Also! The gatherPotentialWords needs to return the type (string * (coord * uint32 * char * int)) list
            
                    
                    
            
            forcePrint (sprintf "All words on the board: %A" allWordsOnTheBoard)
            
            //debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            if move.IsEmpty then
                send cstream (SMPass)
            else
                send cstream (SMPlay move)

            // SMChange
            
            
            let msg = recv cstream
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                
                // 01. Update our hand
                let updatedHand : MultiSet.MultiSet<uint32> =
                    ms
                    |> List.map (fun ((_, _), (id, (_, _))) -> id) // Extract played piece IDs
                    |> List.fold (fun acc x -> MultiSet.removeSingle x acc) st.hand // Remove played pieces from hand
                    |> fun hand -> List.fold (fun acc (x, _) -> MultiSet.add x 1u acc) hand newPieces
                
                forcePrint(sprintf "Updated hand: %A \n" (PlayMaker.multisetToChar updatedHand))
                
                // 02. Update the board
                let updatedBoard = insertMovesIntoState ms st.piecesOnBoard
                
                let st' = mkState st.board st.dict st.playerNumber updatedHand updatedBoard
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                
                let updatedBoard = insertMovesIntoState ms st.piecesOnBoard
                
                let st' = mkState st.board st.dict st.playerNumber st.hand updatedBoard
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMPassed (pid)) ->
                (* Passed *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMChangeSuccess (ms)) ->
                (* Successfully swapped pieces *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
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

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet Map.empty)
        