[<AutoOpen>]
module Partas.Feliz.Generator.Types
open System.ComponentModel
/// <summary>
/// This info is used internally to construct Fantomas expressions
/// </summary>
type internal ExprInfo = {
    PropertyName: string
    ParamName: string
}

[<RequireQualifiedAccess>]
type InterfaceAttributeEnumType =
    | String of string
    | Integer of int
    | Float of float
    | Boolean of bool

[<EditorBrowsable(EditorBrowsableState.Advanced)>]
[<RequireQualifiedAccess>]
type InterfaceAttributeType =
    | Simple of string
    | Enum of InterfaceAttributeEnumType list
    | Object of string
    | NestedObject of string
    | Funcify of string
    | ListToSeq of string
    | ListToArray of string
    
type InterfaceAttributeTypes =
    | Simple of attrName: string * types: InterfaceAttributeType list
    | Mixed of attrName: string * types: InterfaceAttributeType list
    | Enum of attrName: string * types: InterfaceAttributeEnumType list

module InterfaceAttributeTypes =
    let unsafeSimpleAttrPairs (attributeTypes: InterfaceAttributeTypes list) =
        attributeTypes |> List.collect (function
            | InterfaceAttributeTypes.Simple(attrName, types) ->
                types |> List.map (fun typ -> attrName, typ)
            | e -> failwithf $"Cannot unwrap non simple attribute types: %A{e}"
        )
    
/// <summary>
/// Defines a type definition. Predetermines whether the type will contain enum properties,
/// which requires a module of the same name to be built.
/// </summary>
[<EditorBrowsable(EditorBrowsableState.Advanced)>]
[<RequireQualifiedAccess>]
type InterfaceType =
    | Simple of typName: string * attrs: InterfaceAttributeTypes list
    | Mixed of typName: string * attrs: InterfaceAttributeTypes list
    
module InterfaceType =
    let create typName (attrs: InterfaceAttributeTypes list) =
        let isComplex =
            attrs
            |> List.exists (_.IsSimple >> not)
        if isComplex then
            InterfaceType.Mixed(typName, attrs)
        else
            InterfaceType.Simple(typName, attrs)
    let private deconstruct = function
        | InterfaceType.Simple(typName, attrs)
        | InterfaceType.Mixed(typName, attrs) -> typName,attrs
    let attrs = deconstruct >> snd
    let typeName = deconstruct >> fst
    
type InterfaceType with
    static member Create(typeName, attrs) = InterfaceType.create typeName attrs
    member this.Attributes = this |> InterfaceType.attrs
    member this.TypeName = this |> InterfaceType.typeName

type AttributeReturnTypes = {
    /// <summary>
    /// Mapping of the type name to the property type name.
    /// Default adds an 'I' prefix and 'Prop' postfix.
    /// </summary>
    Mapping: string -> string
    /// <summary>
    /// The type is defined elsewhere. We use the provided name as the return type,
    /// but we do not construct the definition.
    /// Default is true.
    /// </summary>
    ExternDefn: bool
} with
    static member inline Default = {
        Mapping = fun input -> $"I{input}Prop"
        ExternDefn = false
    }

/// <summary>
/// The configuration for a generator schema.
/// </summary>
type Config = {
    /// <summary>
    /// The configuration and handling of the return types
    /// for attributes of a type.
    /// </summary>
    AttributeReturnTypes: AttributeReturnTypes
    /// <summary>
    /// Whether to automatically strop enums that aren't strings. Default
    /// is true.
    /// </summary>
    StropNotStringEnums: bool
} with
    static member inline Default = {
        AttributeReturnTypes = AttributeReturnTypes.Default
        StropNotStringEnums = true
    }

type Schema = {
    Config: Config
    Namespace: string option
    RequiredOpens: string list
    Interfaces: InterfaceType list
} with
    static member inline Default = {
        Config = Config.Default
        Namespace = None
        RequiredOpens = []
        Interfaces = []
    }
    static member inline Empty = {
        Config = {
            AttributeReturnTypes = {
                ExternDefn = false
                Mapping = id
            }
            StropNotStringEnums = true
        }
        RequiredOpens = []
        Interfaces = []
        Namespace = None
    }
