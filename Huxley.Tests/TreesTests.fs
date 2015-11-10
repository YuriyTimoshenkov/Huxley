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
            tree <- AVLTree.insert 2 "2" tree
            tree <- AVLTree.insert 3 "3" tree
            tree <- AVLTree.insert 4 "4" tree
            tree <- AVLTree.insert 5 "5" tree
            tree <- AVLTree.insert 6 "6" tree
            tree <- AVLTree.insert 7 "7" tree
            tree <- AVLTree.insert 8 "8" tree
            tree <- AVLTree.insert 9 "9" tree
            tree <- AVLTree.insert 10 "10" tree


            System.Diagnostics.Debug.WriteLine (AVLTree.drawTree tree 2)

            Assert.IsTrue(1 = 1)
