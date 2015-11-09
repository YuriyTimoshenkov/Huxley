namespace Huxley.TreesTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open Huxley.Core
open Huxley.Core.Trees

module TreesTests =

    [<TestClass>]
    type TreesTests() = 

    //    [<TestInitialize>]
    //    member x.setup() = printf ""
    //        //your setup code

        [<TestMethod>]
        member x.RemoveNodeSuccess() =
            
            let mutable tree = AVLTree.Empty

            tree <- AVLTree.insert 1 "1" tree 

            let length = match tree with 
                            | AVLTree.Empty -> 0 
                            | AVLTree.Node(h, _, _, _) -> h

            let bb = length = 1
            Assert.IsTrue(bb)
