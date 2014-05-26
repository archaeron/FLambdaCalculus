namespace Tests
open System
open NUnit.Framework
open Lambda.Eval

[<TestFixture>]
type Test() = 

    [<Test>]
    member x.TestFreeVariables() =
        
        let term = Application (Variable "y", Lambda ("x", Constant Succ))
        let freeVariables = freeVariables term
        Assert.AreEqual (freeVariables, ["y"])

    [<Test>]
    member x.TestSubstitution() =
        let term1 = Application (Variable "y", Lambda ("x",Constant Succ))
        let sub1 = substitution term1 "y" (Constant Succ)
        let goal1 = Application (Constant Succ, Lambda ("x",Constant Succ))
        Assert.AreEqual (sub1, goal1)


        let term2 = Lambda ("y", Variable "x")
        let sub2 = substitution term2 "x" (Application (Variable "y", Lambda ("x", Constant Succ)))
        let goal2 = Lambda ("new-var", (Application (Variable "y", Lambda ("x", Constant Succ))))
        Assert.AreEqual (sub2, goal2)



    [<Test>]
    member x.TestEtaConversion() =
        let term1 = Lambda("x", Application(Constant Succ, Variable "x"))
        let conv1 = etaConversion term1
        let goal1 = Constant Succ
        Assert.AreEqual (conv1, goal1)

        let term2 = Lambda("x", Application(Constant (Num 5), Variable "x"))
        let conv2 = etaConversion term2
        let goal2 = Lambda("x", Application(Constant (Num 5), Variable "x"))
        Assert.AreEqual (conv2, goal2)



    [<Test>]
    member x.TestDeltaConversion() =
        let term1 = Lambda("x", Application(Constant Succ, Variable "x"))
        let conv1 = deltaConversion term1
        let goal1 = Lambda("x", Application(Constant Succ, Variable "x"))
        Assert.AreEqual (conv1, goal1)

        let term2 = Application(Constant Succ, Constant (Num 5))
        let conv2 = deltaConversion term2
        let goal2 = Constant (Num 6)
        Assert.AreEqual (conv2, goal2)

        let term3 = Application (Application (Constant Add, Constant (Num 5)), Constant (Num 3))
        let conv3 = deltaConversion term3
        let goal3 = Constant (Num 8)
        Assert.AreEqual (conv3, goal3)
