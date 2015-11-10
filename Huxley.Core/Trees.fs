namespace Huxley.Core.Trees

module AVLTree =
    open System

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

    let rec findPredecessor = function
        | Empty -> Empty
        | Node(_, l, _, Empty) as node when height l = 1 -> node
        | Node(_, Empty, _, Empty) as node -> node
        | Node(_, l, x, r) -> findPredecessor r 

    let rec removePredecessor = function
        | Empty -> Empty
        | Node(_, Node(h,_,lv,_), _, Empty) as node when h = 1 -> createNode Empty lv Empty
        | Node(_, Empty, _, Empty) -> Empty
        | Node(_, l, x, r) -> createNode l x (removePredecessor r)

    let rec findSuccessor = function
        | Empty -> Empty
        | Node(_, Empty, _, r) as node when height r = 1 -> node
        | Node(_, Empty, _, Empty) as node -> node
        | Node(_, l, x, r) -> findPredecessor l 

    let rec removeSuccessor  = function
        | Empty -> Empty
        | Node(_, Empty, _, Node(h,_,rv,_)) as node when h = 1 -> createNode Empty rv Empty
        | Node(_, Empty, _, Empty) -> Empty
        | Node(_, l, x, r) -> createNode (removePredecessor l) x r


    let rec remove key = function
        | Empty -> Empty
        | Node(_, l, x, r) as node ->
            if key = x.Key then 
                if balanceFactor node >= 0
                then 
                    let predecessor = findPredecessor l
                    match predecessor with
                    | Node(_, _, xp, _) -> createNode (removePredecessor l) xp r 
                    | Empty -> Empty
                else
                    let successor  = findSuccessor  r
                    match successor with
                    | Node(_, _, xp, _) -> createNode l xp (removeSuccessor  r) 
                    | Empty -> Empty
            else
                let l', r' = if key < x.Key then 
                                   remove key l, r 
                             else 
                                   l, remove key r
                createNode l' x r'

    let rec getValue key = function
        | Empty -> Option.None
        | Node(_, l, x, r) ->
            if key = x.Key then Option.Some x.Value
            else
                if key < x.Key then getValue key l
                else getValue key r

    let drawTree tree cellSize = 
            let treeHeigh = height tree

            //Fill empty canvas
            let stringCanvas = [ for i in 1 ..treeHeigh  -> String.Empty ]
            let updateListItem = fun list index newValue  -> List.mapi ( fun i item -> if i= index then item + newValue else item ) list

            let rec drawTree drawContext vlevel hLevel x = 
                match x with
                | Empty -> drawContext
                | Node(h, l, x, r) as node -> 
                    drawTree 
                                (drawTree 
                                    (updateListItem drawContext vlevel (String.replicate (hLevel - drawContext.[vlevel].Length) " " + x.Key.ToString()))
                                    (vlevel + 1)
                                    (hLevel - ((pown 2 h - 2) / 2)) 
                                    l) 
                                (vlevel + 1)
                                (hLevel + ((pown 2 h + 2) / 2) - 1) 
                                r
            
            let callSize = 2
            List.fold(fun state item -> state + item + Environment.NewLine) String.Empty (drawTree stringCanvas 0 ((pown 2 treeHeigh) * callSize / 2) tree)
    