namespace Huxley.Trees

module AVLTree =

    type TreeNodeContent<'TKey, 'TValue> = {Key: 'TKey; Value: 'TValue }

    type Tree<'TKey, 'TValue> =
        | Empty
        | Node of int * Tree<'TKey, 'TValue> * TreeNodeContent<'TKey, 'TValue> * Tree<'TKey, 'TValue>  

    let height = function
        | Node(h, _, _, _) -> h
        | Empty -> 0

    let createNode leftNode value rightNode = 
        Node(1 + max (height leftNode) (height rightNode), leftNode, value, rightNode)

    let rotateRight = function
        | Node(_, Node(_, ll, lx, lr), x, r) ->
            let r' = createNode lr x r
            createNode ll lx r'
        | node -> node
    
    let rotateLeft = function
        | Node(_, l, x, Node(_, rl, rx, rr)) ->
            let l' = createNode l x rl
            createNode l' rx rr
        | node -> node
    
    let rotateRightLeft = function
        | Node(h, l, x, r) ->
            let r' = rotateRight r
            let node' = createNode l x r'
            rotateLeft node'
        | node -> node
    
    let rotateLeftRight = function
        | Node(h, l, x, r) ->
            let l' = rotateLeft l
            let node' = createNode l' x r
            rotateRight node'
        | node -> node
    
    let balanceFactor = function
        | Empty -> 0
        | Node(_, l, _, r) -> (height l) - (height r)
    
    let balance = function
        (* left unbalanced *)
        | Node(h, l, x, r) as node when balanceFactor node >= 2 ->
            if balanceFactor l >= 1 then rotateRight node      (* left left case *)
            else rotateLeftRight node                        (* left right case *)
        (* right unbalanced *)
        | Node(h, l, x, r) as node when balanceFactor node <= -2 ->
            if balanceFactor r <= -1 then rotateLeft node      (* right right case *)
            else rotateRightLeft node                         (* right left case *)
        | node -> node
    
    let rec insert key value = function
        | Empty -> Node(1, Empty, {Key = key; Value = value}, Empty)
        | Node(_, l, x, r) as node ->
            if key = x.Key then node
            else
                let l', r' = if key < x.Key then insert key value l, r else l, insert key value r
                let node' = createNode l' x r'
                balance <| node'
            
    let rec containsKey key = function
        | Empty -> false
        | Node(_, l, x, r) ->
            if key = x.Key then true
            else
                if key < x.Key then containsKey key l
                else containsKey key r

    let rec getValue key = function
        | Empty -> Option.None
        | Node(_, l, x, r) ->
            if key = x.Key then Option.Some x.Value
            else
                if key < x.Key then getValue key l
                else getValue key r
    