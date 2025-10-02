# Partas.Feliz.Generator

> [!CAUTION]
> This is heavily WIP and currently not suitable for public use outside of contribution/testing

A ~~simple~~  ~~cool~~ stupid generator using Fantomas and a previous script I made for refining Glultinum bindings.

Allows us to use a fsx script as a source of truth, with a simplified scheme of defining Feliz bindings, and then generating the file from that.

## Goals

Maximize lazy cat vibes.

The 'DSL' is simply supposed to require less effort than manually creating the bindings. It can also be used as an intermediary for formats like `yaml`, or others.

## Current API

> [!WARNING]
> API is constantly changing, check the tests to see what does what until I settle on something

```fsharp
// The definition of a file is done by building a schema
type Schema = {
    // Manipulate how things are generated
    Config: Config
    // The namespace for the file
    Namespace: string option
    // The modules to open at the head of the file
    RequiredOpens: string list
    // Our definitions for feliz style
    Interfaces: InterfaceType list
}
```

---

Interfaces are created by using the `makeType` utility in combination with the type name and a list of attribute definitions

```fsharp
let interfaces: InterfaceType list = [
    makeType "ParamOptions" [
        // attributes go here
    ]
]
```

---

Attributes are easily made using the `=>>` for properties with only one accepted type, and `==>` to pass a list of accepted types

```fsharp
makeType "ParamOptions" [
    "testAttribute" =>> nameof float //same as "float"
    "testListAttr" ==> [
        nameof float
        nameof int
        nameof string
    ]
]
```

---

The real help here is the definition of enums using feliz style. Simply add a sublist of the string enum options

```fsharp
makeType "ParamOptions" [
                    // enums
    "testEnum" ==> [[ "center"; "right" ]]
    "testMix" ==> [
        // Special constant that is equivalent to
        // yield! [ nameof float; nameof int ]
        number
        // Special constant that is equivalent to
        // yield! [ "float array"; "int array" ]
        numberArray
        [ // enums
            "center"
            "right"
            // you can even do numbers
            // note: above string would need to also be boxed
            box 5
            box 3.2
            box true
        ]
    ]
]
```

---

Or maybe you want to define an attribute which builds another object from a list:

```fsharp
makeType "ParamOptions" [
    "label" ==> [
        "string list"
        nameof string
        object "ParamOptions"
    ]
]
```

---

Now you can build the schema (use `Schema.buildToFile` to directly write to a file).

```fsharp
{
    Interfaces = [
        makeType "SomeType" [
            "attr" ==> [
                nameof float
                nameof int
                [ // sublist = 'enums'
                    "x"
                    "y"
                ]
            ]
            "otherAttr" ==> [
                nameof string
            ]
        ]
    ]
} |> Schema.build
```

```fsharp

type ISomeTypeProp =
    interface end

[<Erase>]
module SomeType =
    [<Erase>]
    type attr =
        static member inline ofFloat(value: float) : ISomeTypeProp = unbox( "attr", value)
        static member inline ofInt(value: int) : ISomeTypeProp = unbox( "attr", value)
        static member inline x: ISomeTypeProp = unbox("attr", "x")
        static member inline y: ISomeTypeProp = unbox("attr", "y")

[<Erase>]
type SomeType =
    static member inline otherAttr(value: string) : ISomeTypeProp = unbox("otherAttr", value)
```

## Non-Goals

We aren't intending to do EVERYTHING using the generator, just the attributes and annoying tedious template work.

We do want to support things like documenting props and adding the ability to alter how type names are handled/generated.
