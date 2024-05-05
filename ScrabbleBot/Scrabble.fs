namespace ScrabBoyz

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open GameState
open PlayMaker

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
    let mkState b d pn h pob = {
        GameState.board = b
        GameState.dict = d
        GameState.playerNumber = pn
        GameState.hand = h
        GameState.piecesOnBoard = pob;
    }
    
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
    
    let playGame cstream pieces (st : state) =
        let rec aux (st : state) =
            Print.printHand pieces (st.hand )

            //////////////////////////////////// BOT PLAY ////////////////////////////////////
           
            let longestWord = longestWordWeCanPlay st 
            
            forcePrint (sprintf "\n================\nLongest word :: %s\n================\n" (fst longestWord))
            
            //////////////////////////////////////////////////////////////////////////////////
            
            //////////////////////////////////// REMOVE THIS /////////////////////////////////
            
            let input =  System.Console.ReadLine()
            let move = RegEx.parseMove input

            // debugPrint (sprintf "Player %d -> Server:\n%A\n" (st.playerNumber) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            // debugPrint (sprintf "Player %d <- Server:\n%A\n" (st.playerNumber) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                
                // 01. Update our hand
                let updatedHand : MultiSet.MultiSet<uint32> =
                    ms
                    |> List.map (fun ((_, _), (id, (_, _))) -> id) // Extract played piece IDs
                    |> List.fold (fun acc x -> MultiSet.removeSingle x acc) st.hand // Remove played pieces from hand
                    |> fun hand -> List.fold (fun acc (x, _) -> MultiSet.add x 1u acc) hand newPieces
                                
                // 02. Update the board
                let updatedBoard = insertMovesIntoState ms st.piecesOnBoard
                
                let st' = mkState st.board st.dict st.playerNumber updatedHand updatedBoard
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                
                let updatedBoard = insertMovesIntoState ms st.piecesOnBoard
                
                forcePrint(sprintf "\n!Opponent played and the board looks as follows %A !\n" updatedBoard)
                
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
        