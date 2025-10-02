# Partas.Feliz.Generator

> [!CAUTION]
> This is heavily WIP and currently not suitable for public use outside of contribution/testing

A simple generator using Fantomas and a previous script I made for refining Glultinum bindings.

Allows us to use a fsx script as a source of truth, with a simplified scheme of defining Feliz bindings, and then generating the file from that.

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
