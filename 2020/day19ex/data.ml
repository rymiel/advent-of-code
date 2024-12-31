type expr = Chr of char | List of int list | Alternate of int list * int list
type rule = int * expr
