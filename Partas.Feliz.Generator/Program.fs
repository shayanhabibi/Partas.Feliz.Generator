namespace Partas.Feliz.Generator

open System.Collections.Generic
open System.IO
open FSharp.Core
open Fantomas.Core
open Fantomas.FCS
open Fantomas.FCS.Text
open Fantomas.Core.SyntaxOak
open YamlDotNet.Serialization
open YamlDotNet.Serialization.NamingConventions

[<AutoOpen>]
module Scaffold =
    // Attributable to Shmew - taken from Feliz.Generator.MaterialUI/Common.fs
    let appendApostropheToReservedKeywords =
      let reserved =
        [
          "checked"; "static"; "fixed"; "inline"; "default"; "component";
          "inherit"; "open"; "type"; "true"; "false"; "in"; "end"; "global"
          "use"
        ]
        |> Set.ofList
      fun s -> if reserved.Contains s then s + "'" else s
      
    type SingleTextNode with
        static member make text = SingleTextNode(text, Range.Zero)
        static member makeOptional (text: string) = SingleTextNode.make $"?{text}"
        member this.MakeOptional with get() =
            this.Text |> SingleTextNode.makeOptional
    type MultipleTextsNode with
        static member make texts = MultipleTextsNode([ for text in texts do SingleTextNode.make text ], Range.Zero)
        static member make text = SyntaxOak.MultipleTextsNode.make [ text ]
    type IdentifierOrDot with
        static member makeIdent text = IdentifierOrDot.Ident <| SingleTextNode.make text
        static member makeDot with get() = IdentifierOrDot.KnownDot <| SingleTextNode.make "."
        static member ListFromString (text: string) =
            text.Split '.'
            |> Array.map IdentifierOrDot.makeIdent
            |> fun arr ->
                let length = arr.Length
                arr
                |> Array.mapi (fun idx ident ->
                        if idx = length then
                            [ ident ]
                        else
                            [ ident; IdentifierOrDot.makeDot ]
                    )
            |> Array.toList
            |> List.collect id
    type IdentListNode with
        static member make (text: string) = IdentListNode([ IdentifierOrDot.makeIdent text ], Range.Zero)
        static member make (text: string list) = IdentListNode(text |> List.map IdentifierOrDot.makeIdent, Range.Zero)
        member this.identList with get() =
            this.Content
            |> List.choose (function
                | IdentifierOrDot.Ident ident ->
                    ident.Text
                    |> Some
                | _ -> None
                )
        member this.firstIdent with get() =
            this.identList |> List.head
        member this.lastIdent with get() =
            this.identList |> List.last
        member this.isStropped = this.firstIdent |> _.StartsWith('`')
        member this.isKebab =
            this.Content
            |> List.exists(function
                | IdentifierOrDot.Ident ident ->
                    ident.Text.Contains('-')
                | IdentifierOrDot.KnownDot ident ->
                    ident.Text.Contains('-')
                | _ -> false
                )
        member this.toCamelCase =
            this.Content
            |> List.map(function
                | IdentifierOrDot.Ident ident
                | IdentifierOrDot.KnownDot ident ->
                    ident.Text
                | _ -> ""
                    )
            |> List.toArray
            |> String.concat ""
            |> _.Trim('`')
            |> _.Split('-')
            |> Array.mapi(
                fun i str ->
                    if i = 0 then str
                    else
                    let chars = str.ToCharArray()
                    chars[0] <- chars[0] |> System.Char.ToUpperInvariant
                    chars |> string
                )
            |> String.concat ""
    type AttributeNode with
        static member make text =
            AttributeNode( IdentListNode.make text, None, None, Range.Zero )
    type AttributeListNode with
        static member makeOpening with get() = SingleTextNode.make "[<"
        static member makeClosing with get() = SingleTextNode.make ">]"
        static member make texts =
            AttributeListNode(
                AttributeListNode.makeOpening,
                [ for text in texts do AttributeNode.make text ],
                AttributeListNode.makeClosing,
                Range.Zero
                )
        static member make text =
            AttributeListNode.make [ text ]
    type MultipleAttributeListNode with
        static member make (texts: string seq seq) =
            MultipleAttributeListNode([ for text in texts do AttributeListNode.make text ], Range.Zero)
        static member make (text: string seq)= MultipleAttributeListNode.make [ text ]
        static member make (text: string)= MultipleAttributeListNode.make [ text ]
    type PatParenNode with
        static member makeOpening with get() = SingleTextNode.make "("
        static member makeClosing with get() = SingleTextNode.make ")"
        static member make pattern = PatParenNode(PatParenNode.makeOpening, pattern, PatParenNode.makeClosing, Range.Zero)
    type PatParameterNode with
        static member make ident typ = PatParameterNode(None, Pattern.Null(ident), typ, Range.Zero)
    type PatTupleNode with
        static member makeItem (node: SingleTextNode): Choice<Pattern, SingleTextNode> = Choice2Of2 node
        static member makeItem (node: Pattern): Choice<Pattern, SingleTextNode> = Choice1Of2 node
        static member makeConstructor (members: (SingleTextNode * Type) list) =
            PatTupleNode(
                [ for ident,typ in members do
                    yield Pattern.Parameter(PatParameterNode.make ident.MakeOptional (Some typ)) |> PatTupleNode.makeItem
                    yield SingleTextNode.make "," |> PatTupleNode.makeItem
                ] |> List.cutOffLast, Range.Zero
                )
    type MemberDefnInheritNode with
        /// Gets the last identifier in a chain which should be the type without accessors
        member this.tryGetIdentifier with get() =
            let rec getIdent: Node -> string option = function
                | :? IdentListNode as ident ->
                    ident.lastIdent
                    |> Some
                | :? TypeAppPrefixNode as node ->
                    getIdent node.Children[0]
                | _ -> None
            this.Children[1] |> getIdent
    type ITypeDefn with
        member this.getInheritMembers with get() =
            this.Members
            |> List.choose (function MemberDefn.Inherit node -> Some node | _ -> None)
        member this.getInterfaceMembers with get() =
            this.Members
            |> List.choose (function MemberDefn.Interface node -> Some node | _ -> None)
        member this.getAbstractMembers with get() =
            this.Members
            |> List.choose(function MemberDefn.AbstractSlot node -> Some node | _ -> None)
        member this.getIdentifier = this.TypeName.Identifier.lastIdent
        member this.getAttributes = this.TypeName.Attributes
        member this.getInheritMembersIdentifiers with get() =
            this.getInheritMembers
            |> List.map _.tryGetIdentifier
            |> List.choose id
    type TypeDefnRegularNode with
        member this.TypeDefn = this :> ITypeDefn
    type XmlDocNode with
        static member make (docs: string seq) =
            XmlDocNode([|
                yield "/// <summary>"
                for line in docs do
                    for subline in
                        line |> Seq.chunkBySize 120
                        |> Seq.map (string >> sprintf "/// %s")
                        do yield subline
                yield "/// </summary>"
            |], Range.Zero)
    type MemberDefnInheritNode with
        static member make identifier =
            MemberDefnInheritNode(
                SingleTextNode.make "inherit",
                Type.LongIdent (IdentListNode.make identifier),
                Range.Zero
                )
    type MemberDefnAbstractSlotNode with
        member this.getIdentifierTypeTuple =
            this.Identifier.Text, this.Type
        static member makeSimple(
            identifier: string,
            typ: Type,
            ?docs: string seq,
            ?attributes: string seq,
            ?withGetSet: bool
            ) =
            let withGetSet = defaultArg withGetSet false
            MemberDefnAbstractSlotNode(
                docs |> Option.map XmlDocNode.make,
                attributes |> Option.map MultipleAttributeListNode.make,
                MultipleTextsNode.make ["abstract"; "member"],
                SingleTextNode.make identifier,
                None,
                typ,
                if withGetSet then
                    MultipleTextsNode.make [ "with"; "get,"; "set" ]
                    |> Some
                else None
                ,Range.Zero
                )
    type MemberDefn with
        static member makeInherit identifier =
            MemberDefnInheritNode.make identifier |> MemberDefn.Inherit
        static member makeAbstract (attrName: string, typ: Type) =
            MemberDefnAbstractSlotNode.makeSimple(attrName, typ)
            |> MemberDefn.AbstractSlot
        static member makeExtensionGetSetWith (name: string, typ: Type, ?inlineOverload: string) =
            MemberDefnPropertyGetSetNode(
                None,Some(MultipleAttributeListNode.make "Erase"),MultipleTextsNode.make "member",None,None,
                IdentListNode.make $"_.{name}", SingleTextNode.make "with",
                PropertyGetSetBindingNode(
                      None, None, None, SingleTextNode.make "set", [
                          Pattern.Parameter(PatParameterNode.make (SingleTextNode.make "_") (Some typ))
                          |> PatParenNode.make
                          |> Pattern.Paren
                      ], None, SingleTextNode.make "=", Expr.Null(SingleTextNode.make "()"), Range.Zero
                    ),
                SingleTextNode.make "and" |> Some,
                PropertyGetSetBindingNode(
                    None,
                    MultipleAttributeListNode.make "Erase" |> Some,
                    None, SingleTextNode.make "get", [
                        Pattern.Unit(UnitNode(PatParenNode.makeOpening, PatParenNode.makeClosing, Range.Zero))
                    ],
                    BindingReturnInfoNode(SingleTextNode.make ":", typ, Range.Zero)
                    |> Some,
                    SingleTextNode.make "=",
                    Expr.Ident(SingleTextNode.make "JS.undefined"),
                    Range.Zero
                    ) |> Some,
                Range.Zero
                ) |> MemberDefn.PropertyGetSet
        static member makeExtensionGetSet (name: string) (typ: Type) =
            MemberDefn.makeExtensionGetSetWith(name, typ)
    type TypeNameNode with
        static member makeSimple (
            identifier: string,
            ?docs: string list,
            ?attributes: string list,
            ?suffix: string
            ) =
            let identifier = appendApostropheToReservedKeywords identifier
            TypeNameNode(
                docs |> Option.map XmlDocNode.make,
                attributes |> Option.map MultipleAttributeListNode.make,
                SingleTextNode.make "type",
                None,
                IdentListNode.make identifier,
                None,
                [],
                None,
                SingleTextNode.make "=" |> Some,
                suffix |> Option.map SingleTextNode.make,
                Range.Zero
                )
        static member makeExtension (identifier: IdentListNode) =
            TypeNameNode(None,None,SingleTextNode.make "type", None,
                         identifier, None, [], None,
                         SingleTextNode.make "with" |> Some, None, Range.Zero)
        static member makeExtension (identifier: string) =
            IdentListNode.make identifier |> TypeNameNode.makeExtension
    type TypeDefnRegularNode with
        static member make members typeNameNode =
            TypeDefnRegularNode(typeNameNode, members, Range.Zero)
    type ModuleDecl with
        static member wrapInNestedModule name (attributes: string seq option) decls =
            NestedModuleNode(
                None, attributes |> Option.map MultipleAttributeListNode.make,
                SingleTextNode.make "module",None,false,IdentListNode.make name,
                SingleTextNode.make "=", decls, Range.Zero
                )
            |> ModuleDecl.NestedModule
    type BindingReturnInfoNode with
        static member make typ =
            BindingReturnInfoNode(
                colon = SingleTextNode.make ":",
                t = typ,
                range = Range.Zero
                )
        static member make typIdent =
            SingleTextNode.make typIdent
            |> Type.Anon
            |> BindingReturnInfoNode.make
    type Expr with
        static member make text =
            SingleTextNode.make text
            |> Expr.Ident
    type ExprInfo = {
        FunctionName: string
        ParamName: string
    }
    type BindingNode with
    
        static member makeEnum (functionName, ?attrName: string, ?returnType: string , ?isInline: bool, ?isStatic: bool) =
            let isStatic = defaultArg isStatic true
            let isInline = defaultArg isInline true
            let leadingKeyword =
                if isStatic then
                    MultipleTextsNode.make [ "static"; "member" ]
                else
                    MultipleTextsNode.make [ "member" ]
            let expr =
                match attrName with
                | Some attrName ->
                    $"unbox(\"{attrName}\", \"{functionName}\")"
                | _ -> $"unbox \"{functionName}\""
                |> SingleTextNode.make
                |> Expr.Ident
            let functionName = appendApostropheToReservedKeywords functionName
            BindingNode(
                xmlDoc = None,
                attributes = None,
                leadingKeyword = leadingKeyword,
                isMutable = false,
                inlineNode = (if isInline then SingleTextNode.make "inline" |> Some else None),
                accessibility = None,
                functionName =  Choice1Of2 (IdentListNode.make functionName),
                genericTypeParameters = None,
                parameters = [ ],
                returnType = (returnType |> Option.map BindingReturnInfoNode.make),
                equals = SingleTextNode.make "=",
                expr = expr,
                range = Range.Zero
                )
        static member make (functionName, paramType: string, expr: ExprInfo -> string, ?returnType: string ,?paramName, ?isInline: bool, ?isStatic: bool) =
            let isStatic = defaultArg isStatic true
            let isInline = defaultArg isInline true
            let paramName = defaultArg paramName "value"
            let leadingKeyword =
                if isStatic then
                    MultipleTextsNode.make [ "static"; "member" ]
                else
                    MultipleTextsNode.make [ "member" ]
            let functionName = appendApostropheToReservedKeywords functionName
            BindingNode(
                xmlDoc = None,
                attributes = None,
                leadingKeyword = leadingKeyword,
                isMutable = false,
                inlineNode = (if isInline then SingleTextNode.make "inline" |> Some else None),
                accessibility = None,
                functionName =  Choice1Of2 (IdentListNode.make functionName),
                genericTypeParameters = None,
                parameters = [
                    (SingleTextNode.make paramName, SingleTextNode.make paramType |> Type.Anon |> Some)
                    ||> PatParameterNode.make
                    |> Pattern.Parameter
                    |> PatParenNode.make
                    |> Pattern.Paren
                ],
                returnType = (returnType |> Option.map BindingReturnInfoNode.make),
                equals = SingleTextNode.make "=",
                expr = Expr.make (expr { ParamName = paramName; FunctionName = functionName }),
                range = Range.Zero
                )
        static member makeSimple (functionName, paramType, ?returnType: string) =
            let expr = fun paramName -> $"unbox(\"{paramName.FunctionName}\", {paramName.ParamName})"
            BindingNode.make(functionName, paramType, expr, ?returnType = returnType)
        static member makeObject (functionName, paramType) =
            let expr = fun paramName -> $"unbox(\"{paramName.FunctionName}\", createObj (unbox {paramName.ParamName}))"
            BindingNode.make(functionName, paramType, expr)
    type NestedModuleNode with
        static member make (name: string) decls =
            let name = appendApostropheToReservedKeywords name
            NestedModuleNode(
                xmlDoc = None,
                attributes = Some(MultipleAttributeListNode.make "Erase"),
                moduleKeyword = SingleTextNode.make "module",
                accessibility = None,
                isRecursive = false,
                identifier = IdentListNode.make name,
                equalsNode = SingleTextNode.make "=",
                decls = decls,
                range = Range.Zero
                )
    let finaliseTypes (e: TypeDefnRegularNode list) =
        let decls = e |> List.map (TypeDefn.Regular >> ModuleDecl.TypeDefn)
        Oak([], [ ModuleOrNamespaceNode(None, decls, Range.Zero) ], Range.Zero)
        |> CodeFormatter.FormatOakAsync
        |> Async.RunSynchronously
        |> printfn "%s"
[<RequireQualifiedAccess>]
type InterfaceAttributeType =
    | Simple of string
    | Enum of string list
type InterfaceAttributeTypes =
    | Simple of attrName: string * types: string list
    | Mixed of attrName: string * types: InterfaceAttributeType list
    | Enum of attrName: string * types: string list
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
type Schema = {
    RootTypeName: string option
    Interfaces: InterfaceType list
}

[<AutoOpen>]
module Operators =
    [<Literal>]
    let number = "%MACRO%number"
    [<Literal>]
    let private NUMBER = number
    let (==>) (x: string) (typs: obj list) =
        let mutable hasSimple: bool = false
        let mutable hasEnums: bool = false
        let mappedTypes =
            typs
            |> List.map (function
                | :? string as s ->
                    hasSimple <- true
                    InterfaceAttributeType.Simple s
                | :? (string list) as s ->
                    hasEnums <- true
                    InterfaceAttributeType.Enum s
                | t -> failwith $"Unhandled AttributeType: {t.GetType()} \n Value: {t}")
            |> List.collect (function
                | InterfaceAttributeType.Simple NUMBER ->
                    [
                        InterfaceAttributeType.Simple "float"
                        InterfaceAttributeType.Simple "int"
                    ]
                | normTyp -> [ normTyp ])
        match hasSimple,hasEnums with
        | true, true ->
            InterfaceAttributeTypes.Mixed(x, mappedTypes)
        | true, _ ->
            let mappedTypes =
                typs
                |> List.map (function
                    | :? string as s -> s
                    | _ -> failwith "UNREACHABLE")
            InterfaceAttributeTypes.Simple(x, mappedTypes)
        | _, true ->
            let mappedTypes =
                typs
                |> List.map (function
                    | :? (string list) as s -> s
                    | _ -> failwith "UNREACHABLE")
            InterfaceAttributeTypes.Enum(x, List.collect id mappedTypes)
        | _ -> InterfaceAttributeTypes.Simple(x, [])
    let (=>>) (x: string) (value: string) =
        match value with
        | NUMBER -> InterfaceAttributeTypes.Simple(x, [ "float"; "int" ])
        | _ -> InterfaceAttributeTypes.Simple(x, [ value ])
    let inline makeType text (attrs: InterfaceAttributeTypes list) = InterfaceType.create text attrs

module Schema =
    let build (schema: Schema) =
        let makePropInterfaceName typName =
            "I" + typName + "Prop"
        let makePropInterfaceType =
            TypeNameNode.makeSimple
            >> TypeDefnRegularNode.make [
                MemberDefn.DoExpr(ExprSingleNode(
                    SingleTextNode.make "interface", true, false,
                    Expr.Ident(SingleTextNode.make "end"), Range.Zero
                    ))
            ]
            >> TypeDefn.Regular
            >> ModuleDecl.TypeDefn
        [
            match schema with
            | { RootTypeName = Some name } ->
                yield makePropInterfaceType name
            | _ -> ()
            for interfaceDef in schema.Interfaces do
                let propInterfaceName = makePropInterfaceName interfaceDef.TypeName
                yield makePropInterfaceType propInterfaceName
                match interfaceDef with
                | InterfaceType.Simple(typName, attrs) ->
                    let members =
                        attrs
                        |> List.collect (function
                            | InterfaceAttributeTypes.Simple(attrName, typs) ->
                                typs
                                |> List.map(fun typ ->
                                    BindingNode.makeSimple(attrName, typ, propInterfaceName)
                                    |> MemberDefn.Member
                                    )
                            | _ -> failwith "CANNOT REACH"
                                )
                    yield
                        TypeNameNode.makeSimple typName
                        |> TypeDefnRegularNode.make members
                        |> TypeDefn.Regular
                        |> ModuleDecl.TypeDefn
                | InterfaceType.Mixed(typName, attrs) ->
                    let simpleAttrs, complexAttrs =
                        attrs |> List.partition _.IsSimple
                    yield
                        complexAttrs
                        |> List.map (function
                            | InterfaceAttributeTypes.Enum(attrName, typs) ->
                                TypeNameNode.makeSimple(attrName, attributes = [ "Erase" ])
                                |> TypeDefnRegularNode.make (
                                    typs
                                    |> List.map (
                                        fun typ -> BindingNode.makeEnum(typ, attrName, returnType = propInterfaceName)
                                        >> MemberDefn.Member
                                        )
                                    )
                            | InterfaceAttributeTypes.Mixed(attrName, complexTyps) ->
                                let simple, enums =
                                    complexTyps |> List.partition _.IsSimple
                                TypeNameNode.makeSimple(attrName, attributes = [ "Erase" ])
                                |> TypeDefnRegularNode.make [
                                    yield!
                                        simple
                                        |> List.map (function
                                            | InterfaceAttributeType.Simple typ ->
                                                let functionName =
                                                    match typ.Length with
                                                    | 1 -> typ.ToUpper()
                                                    | _ ->
                                                        typ.Substring(0,1).ToUpper() + typ.Substring(1)
                                                    |> (+) "of"
                                                let expr = fun (exprInfo: ExprInfo) ->
                                                    $"unbox( \"{attrName}\", {exprInfo.ParamName})"
                                                BindingNode.make(functionName, typ, expr, returnType = propInterfaceName)
                                                |> MemberDefn.Member
                                            | _ -> failwith "UNREACHABLE")
                                    yield!
                                        enums
                                        |> List.collect (function
                                            | InterfaceAttributeType.Enum v ->
                                                let makeEnum typ =
                                                    BindingNode.makeEnum(typ, attrName = attrName, returnType = propInterfaceName)
                                                v |> List.map (makeEnum >> MemberDefn.Member)
                                            | _ -> failwith "UNREACHABLE")
                                ]
                            | _ -> failwith "UNREACHABLE"
                            >> TypeDefn.Regular
                            >> ModuleDecl.TypeDefn
                            )
                        |> NestedModuleNode.make typName
                        |> ModuleDecl.NestedModule
                    yield
                        TypeNameNode.makeSimple(typName, attributes = [ "Erase" ])
                        |> match simpleAttrs with
                            | [] ->
                                TypeDefnRegularNode.make [
                                    MemberDefn.DoExpr( // lol im lazy
                                        ExprSingleNode(
                                            SingleTextNode.make "interface",
                                            true,
                                            false,
                                            Expr.Ident (SingleTextNode.make "end"),
                                            Range.Zero
                                            )
                                        )
                                ]
                            | _ -> 
                                TypeDefnRegularNode.make (
                                    simpleAttrs
                                    |> List.collect (function
                                        | InterfaceAttributeTypes.Simple(attrName, typs) ->
                                            typs
                                            |> List.map (fun typ ->
                                                BindingNode.makeSimple(attrName,typ, propInterfaceName)
                                                |> MemberDefn.Member )
                                        | _ -> failwith "UNREACHABLE"
                                        )
                                    )
                        |> TypeDefn.Regular
                        |> ModuleDecl.TypeDefn
        ] |> fun decls ->
        Oak([], [ ModuleOrNamespaceNode(None, decls, Range.Zero) ], Range.Zero)
        |> CodeFormatter.FormatOakAsync
        |> Async.RunSynchronously
        |> sprintf "%s"
    let buildToFile (filePath: string) =
        build
        >> fun contents ->
            if filePath |> Path.HasExtension then
                File.WriteAllText(filePath, contents)
            else
                failwith $"Cannot write to a filePath without an extension (as it may have been a mistake): %s{filePath}"
