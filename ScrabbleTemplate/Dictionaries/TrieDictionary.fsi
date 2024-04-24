// From implementation of Trie Dict made for the Yellow Exercise 2b
module internal TrieDictionary

    type TrieDict
    
    val empty  : unit   -> TrieDict
    val insert : string -> TrieDict -> TrieDict
    val lookup : string -> TrieDict -> bool
    val step   : char   -> TrieDict -> (bool * TrieDict) option