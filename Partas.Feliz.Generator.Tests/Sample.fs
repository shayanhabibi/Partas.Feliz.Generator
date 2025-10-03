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
                       (InterfaceAttributeTypes.Simple("test", [ InterfaceAttributeType.Simple "float"; InterfaceAttributeType.Simple "int" ]))
            }
            test "numberArray macro" {
                [
                    "test" =>> numberArray
                    "test" ==> [ numberArray ]
                ]
                |> Flip.Expect.allEqual
                    ""
                    (InterfaceAttributeTypes.Simple("test", [ InterfaceAttributeType.Simple "float array"; InterfaceAttributeType.Simple "int array" ]))
            }

        ]
        testList "Schema Build" [
            test "Reserved identifiers" {
                let schema = {
                    Config = Config.Default
                    Namespace = None
                    RequiredOpens = []
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
                    Config = Config.Default
                    Namespace = Some "Test.Namespace"
                    RequiredOpens = []
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
                    Config = Config.Default
                    Namespace = Some "Test.Namespace"
                    RequiredOpens = [
                        "Fable.Core"
                    ]
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
            test "Object Types" {
                let schema = {
                    Config = Config.Default
                    Namespace = None
                    RequiredOpens = [ ]
                    Interfaces = [
                        makeType "Test" [
                            "test" =>> object "Param"
                        ]
                    ]
                }
                let expected = """type ITestProp =
    interface end

[<Erase>]
type Test =
    static member inline test(props: IParamProp list) : ITestProp = unbox("test", createObj (unbox props))"""
                schema
                |> Schema.build
                |> _.Trim()
                |> Flip.Expect.equal "" expected
            }
            
            test "Special Enum Types" {
                let schema = {
                    Config = Config.Default
                    Namespace = None
                    RequiredOpens = [ ]
                    Interfaces = [
                        makeType "Test" [
                            "test" =>> object "Param"
                            "enums" ==> [
                                nameof string
                                [
                                    box 5
                                    box 3
                                    box true
                                    box false
                                ]
                            ]
                        ]
                    ]
                }
                let expected = """type ITestProp =
    interface end

[<Erase>]
module Test =
    [<Erase>]
    type enums =
        static member inline ofString(value: string) : ITestProp = unbox("enums", value)
        static member inline ``5``: ITestProp = unbox("enums", 5)
        static member inline ``3``: ITestProp = unbox("enums", 3)
        static member inline true': ITestProp = unbox("enums", true)
        static member inline false': ITestProp = unbox("enums", false)

[<Erase>]
type Test =
    static member inline test(props: IParamProp list) : ITestProp = unbox("test", createObj (unbox props))"""
                
                schema
                |> Schema.build
                |> _.Trim()
                |> Flip.Expect.equal "" expected
            }
            test "List Types" {
                let schema = {
                    Config = Config.Default
                    Namespace = None
                    RequiredOpens = [ ]
                    Interfaces = [
                        makeType "Test" [
                            "test" ==> [
                                listToArray "Param list"
                                listToSeq "Prop list"
                            ] 
                        ]
                    ]
                }
                let expected = """type ITestProp =
    interface end

[<Erase>]
type Test =
    static member inline test(value: Param list) : ITestProp = unbox("test", value |> List.toArray)
    static member inline test(value: Prop list) : ITestProp = unbox("test", value |> List.toSeq)"""
                schema
                |> Schema.build
                |> _.Trim()
                |> Flip.Expect.equal "" expected
            }

            test "NestedObject Types" {
                let schema = {
                    Config = Config.Default
                    Namespace = None
                    RequiredOpens = [ ]
                    Interfaces = [
                        makeType "Test" [
                            "test" ==> [
                                nestedObject "Duration"
                            ] 
                        ]
                    ]
                }
                let expected = """type ITestProp =
    interface end

[<Erase>]
type Test =
    static member inline test(propsList: IDurationProp list list) : ITestProp =
        unbox("test", propsList |> List.map (unbox<(string * obj) list> >> createObj) |> List.toArray)"""
                schema
                |> Schema.build
                |> _.Trim()
                |> Flip.Expect.equal "" expected
            }

            test "Func Types" {
                let schema = {
                    Config = Config.Default
                    Namespace = None
                    RequiredOpens = [ ]
                    Interfaces = [
                        makeType "Test" [
                            "test" ==> [
                                func "Duration -> unit"
                                func "Drop -> Top -> Drip"
                            ] 
                        ]
                    ]
                }
                let expected = """type ITestProp =
    interface end

[<Erase>]
type Test =
    static member inline test(value: Duration -> unit) : ITestProp = unbox("test", System.Func<_,_> value)
    static member inline test(value: Drop -> Top -> Drip) : ITestProp = unbox("test", System.Func<_,_,_> value)"""
                schema
                |> Schema.build
                |> _.Trim()
                |> Flip.Expect.equal "" expected
            }
            
            ptest "Custom Prop Types" {
                let schema = {
                    Config = Config.Default
                    Namespace = None
                    RequiredOpens = [ ]
                    Interfaces = [
                        makeType "Test" [
                            "test" ==> [
                                customProp "int"
                                customPropObj "Duration"
                                keyStringValueObjects "Duration"
                            ] 
                        ]
                    ]
                }
                let expected = """type ITestProp =
    interface end

[<Erase>]
type Test =
    static member inline test(key: string, value: int) : ITestProp = unbox(key, value)"""
                schema
                |> Schema.build
                |> _.Trim()
                |> Flip.Expect.equal "" expected
            }
        ]
    ]
