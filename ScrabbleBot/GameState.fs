module internal GameState
    open ScrabbleUtil
    open Parser
    
    type state = {
        board         : board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32                          
        hand          : MultiSet.MultiSet<uint32>       
        piecesOnBoard : Map<coord, (char * int)>
        turnCounter   : uint32
        numPlayers    : uint32
        piecesLeft    : uint32
        // timeout       : uint32 option  
    }

