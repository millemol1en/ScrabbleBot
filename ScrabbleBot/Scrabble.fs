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

        if m.Success then
            Some(List.tail [ for g in m.Groups -> g.Value ])
        else
            None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?"

        Regex.Matches(ts, pattern)
        |> Seq.cast<Match>
        |> Seq.map (fun t ->
            match t.Value with
            | Regex pattern [ x; y; id; c; p ] -> ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
            | _ -> failwith "Failed (should never happen)")
        |> Seq.toList

module Print =

    let printHand pieces hand =
        hand
        |> MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State =
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state =
        { board: Parser.board
          dict: ScrabbleUtil.Dictionary.Dict
          playerNumber: uint32
          numPlayers: uint32
          hand: MultiSet.MultiSet<uint32>
          turnCounter: uint32

        }

    let mkState b d np pn h tc =
        { board = b
          dict = d
          numPlayers = np
          playerNumber = pn
          hand = h
          turnCounter = tc }

    let board st = st.board
    let dict st = st.dict

    let numPlayers st = st.numPlayers
    let playerNumber st = st.playerNumber
    let hand st = st.hand

    let turnCounter st = st.turnCounter

module Scrabble =
    open System.Threading

    let changeTurn (st: State.state) numPlayers =
        if st.turnCounter >= numPlayers then
            1u
        else
            st.turnCounter + 1u

    let playGame cstream pieces (st: State.state) =
        let rec aux (st: State.state) =
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint
                "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"

            let input = System.Console.ReadLine()
            let move = RegEx.parseMove input

            debugPrint (sprintf "Turn counter %d\n\n" (State.turnCounter st))

            debugPrint (sprintf "Our hand %A\n\n" (State.hand st))
            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM(CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let updatedHand =
                    ms
                    |> List.map (fun ((_, _), (id, (_, _))) -> id) // Extract played piece IDs
                    |> List.fold (fun acc x -> MultiSet.removeSingle x acc) st.hand // Remove played pieces from hand
                    |> fun hand -> List.fold (fun acc (x, _) -> MultiSet.add x 1u acc) hand newPieces


                let st' =
                    { st with
                        turnCounter = changeTurn st st.numPlayers
                        hand = updatedHand }

                aux st'
            | RCM(CMPlayed(pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' =
                    { st with
                        turnCounter = changeTurn st st.numPlayers } // This state needs to be updated

                aux st'
            | RCM(CMPlayFailed(pid, ms)) ->
                (* Failed play. Update your state *)
                let st' =
                    { st with
                        turnCounter = changeTurn st st.numPlayers } // This state needs to be updated

                aux st'
            | RCM(CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err ->
                printfn "Gameplay Error:\n%A" err
                aux st


        aux st

    let startGame
        (boardP: boardProg)
        (dictf: bool -> Dictionary.Dict)
        (numPlayers: uint32)
        (playerNumber: uint32)
        (turnCounter: uint32)
        (hand: (uint32 * uint32) list)
        (tiles: Map<uint32, tile>)
        (timeout: uint32 option)
        (cstream: Stream)
        =
        debugPrint (
            sprintf
                "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n"
                numPlayers
                playerNumber
                turnCounter
                hand
                timeout
        )

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP

        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict numPlayers playerNumber handSet turnCounter)
