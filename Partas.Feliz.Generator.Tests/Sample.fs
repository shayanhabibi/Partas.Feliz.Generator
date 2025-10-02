module Tests

open Expecto
open Fantomas.Core.SyntaxOak
open Partas.Feliz.Generator

[<Tests>]
let tests =
    testList "Generator" [
        testList "Primitives" [
            
            test "number macro" {
                [
                    "test" =>> number
                    "test" ==> [ number ]
                ]
                |> Flip.Expect.allEqual
                       ""
                       (InterfaceAttributeTypes.Simple("test", [ "float"; "int" ]))
            }
            test "numberArray macro" {
                [
                    "test" =>> numberArray
                    "test" ==> [ numberArray ]
                ]
                |> Flip.Expect.allEqual
                    ""
                    (InterfaceAttributeTypes.Simple("test", [ "float array"; "int array" ]))
            }
            test "Reserved Identifiers in expressions" {
                Expr.make "use"
                |> function
                    | Expr.Ident(value) -> value.Text
                    | _ -> failwith ""
                |> Flip.Expect.equal "" "use"
            }
        ]
        testList "Schema Build" [
            test "Reserved identifiers" {
                let schema = {
                    Namespace = None
                    RequiredOpens = []
                    RootTypeName = None
                    Interfaces = [
                        makeType "Test" [
                            "test" =>> nameof float
                            "use" =>> nameof int
                        ]
                    ]
                }
                let expected = """type ITestProp =
    interface end

[<Erase>]
type Test =
    static member inline test(value: float) : ITestProp = unbox("test", value)
    static member inline use'(value: int) : ITestProp = unbox("use", value)"""
                schema
                |> Schema.build
                |> _.Trim()
                |> Flip.Expect.equal "" expected
            }
            test "Namespace" {
                let schema = {
                    Namespace = Some "Test.Namespace"
                    RequiredOpens = []
                    RootTypeName = None
                    Interfaces = [
                        makeType "Test" [
                            "test" =>> nameof float
                        ]
                    ]
                }
                let expected = """namespace Test.Namespace

type ITestProp =
    interface end

[<Erase>]
type Test =
    static member inline test(value: float) : ITestProp = unbox("test", value)"""
                schema
                |> Schema.build
                |> _.Trim()
                |> Flip.Expect.equal "" expected
            }
            
            test "Namespace and Opens" {
                let schema = {
                    Namespace = Some "Test.Namespace"
                    RequiredOpens = [
                        "Fable.Core"
                    ]
                    RootTypeName = None
                    Interfaces = [
                        makeType "Test" [
                            "test" =>> nameof float
                        ]
                    ]
                }
                let expected = """namespace Test.Namespace

open Fable.Core

type ITestProp =
    interface end

[<Erase>]
type Test =
    static member inline test(value: float) : ITestProp = unbox("test", value)"""
                schema
                |> Schema.build
                |> _.Trim()
                |> Flip.Expect.equal "" expected
            }
        ]
    ]
