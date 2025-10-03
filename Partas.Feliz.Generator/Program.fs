namespace Partas.Feliz.Generator

open System.Collections.Generic
open System.IO
open FSharp.Core
open Fantomas.Core
open Fantomas.FCS
open Fantomas.FCS.Text
open Fantomas.Core.SyntaxOak
open Spec
open Utils
open Types
open Partas.Feliz.Generator.Fantomas

[<AutoOpen>]
module internal Impl =
    type UnshittifiedTypes = {
        HasSimple: bool
        HasEnums: bool
        MappedTypes: InterfaceAttributeType list
    }
    let unshittifyEnums: obj list -> InterfaceAttributeEnumType list = List.map (function
        | :? string as typ -> InterfaceAttributeEnumType.String typ
        | :? int as typ -> InterfaceAttributeEnumType.Integer typ
        | :? float as typ -> InterfaceAttributeEnumType.Float typ
        | :? bool as typ -> InterfaceAttributeEnumType.Boolean typ
        | typ -> failwith $"Unhandled Enum type: {typ} of type {typ.GetType().FullName}"
        )
    
    let unshittifyMacros = List.collect (function
        | InterfaceAttributeType.Simple NUMBER ->
            [ InterfaceAttributeType.Simple "float"; InterfaceAttributeType.Simple "int" ]
        | InterfaceAttributeType.Simple NUMBER_ARRAY ->
            [ InterfaceAttributeType.Simple "float array"; InterfaceAttributeType.Simple "int array" ]
        | InterfaceAttributeType.Simple typ when typ.StartsWith(OBJECT) ->
            [ InterfaceAttributeType.Object (typ.Substring(OBJECT.Length)) ]
        | InterfaceAttributeType.Simple typ when typ.StartsWith(NESTED_OBJECT) ->
            [ InterfaceAttributeType.NestedObject (typ.Substring(NESTED_OBJECT.Length)) ]
        | InterfaceAttributeType.Simple typ when typ.StartsWith(FUNCIFY) ->
            [ InterfaceAttributeType.Funcify (typ.Substring(FUNCIFY.Length)) ]
        | InterfaceAttributeType.Simple typ when typ.StartsWith(LIST_SEQ_TRANSFORM) ->
            [ InterfaceAttributeType.ListToSeq (typ.Substring(LIST_SEQ_TRANSFORM.Length)) ]
        | InterfaceAttributeType.Simple typ when typ.StartsWith(LIST_ARRAY_TRANSFORM) ->
            [ InterfaceAttributeType.ListToArray (typ.Substring(LIST_ARRAY_TRANSFORM.Length)) ]
        | InterfaceAttributeType.Simple typ when typ.StartsWith(CUSTOM_PROP) ->
            [ InterfaceAttributeType.CustomProp (typ.Substring(CUSTOM_PROP.Length)) ]
        | InterfaceAttributeType.Simple typ when typ.StartsWith(CUSTOM_PROP_OBJECT) ->
            [ InterfaceAttributeType.CustomPropObject (typ.Substring(CUSTOM_PROP_OBJECT.Length)) ]
        | InterfaceAttributeType.Simple typ when typ.StartsWith(KEY_STRING_VALUE_OBJECTS) ->
            [ InterfaceAttributeType.KeyStringValueObjects (typ.Substring(KEY_STRING_VALUE_OBJECTS.Length)) ]
        | typ -> [ typ ]
            )
        
    let unshittify (typs: obj list) =
        let mutable hasSimple: bool = false
        let mutable hasEnums: bool = false
        let simple = fun typ ->
            hasSimple <- true
            InterfaceAttributeType.Simple typ
        let enum = fun typ ->
            hasEnums <- true
            unshittifyEnums typ
            |> InterfaceAttributeType.Enum
        typs
        |> List.map(function
            | :? string as typ -> simple typ
            | :? (obj list) as typs -> enum typs
            | :? (string list) as typs -> enum (typs |> List.map box)
            | :? (bool list) as typs -> enum (typs |> List.map box)
            | :? (float list) as typs -> enum (typs |> List.map box)
            | :? (int list) as typs -> enum (typs |> List.map box)
            | typ -> failwith $"Unhandled type value {typ} of type {typ.GetType().FullName}"
            )
        |> fun mappedTypes ->
            {
                HasSimple = hasSimple
                HasEnums = hasEnums
                MappedTypes = mappedTypes |> unshittifyMacros
            }
            
            


[<AutoOpen>]
module Operators =
    let (==>) (x: string) (typs: obj list) =
        match unshittify typs with
        | { HasSimple = true; HasEnums = true; MappedTypes = mappedTypes } ->
            InterfaceAttributeTypes.Mixed(x, mappedTypes)
        | { HasSimple = true; MappedTypes = mappedTypes } ->
            InterfaceAttributeTypes.Simple(x, mappedTypes)
        | { HasEnums = true; MappedTypes = mappedTypes } ->
            let mappedTypes =
                mappedTypes
                |> List.collect (function
                    | InterfaceAttributeType.Enum typs -> typs
                    | _ -> failwith "UNREACHABLE")
            InterfaceAttributeTypes.Enum(x, mappedTypes)
        | _ -> InterfaceAttributeTypes.Simple(x, [])
    let (=>>) (x: string) (value: string) =
        [ InterfaceAttributeType.Simple value ]
        |> unshittifyMacros
        |> fun attrTypes ->
            InterfaceAttributeTypes.Simple(x, attrTypes)
    let inline makeType text (attrs: InterfaceAttributeTypes list) = InterfaceType.create text attrs

module Schema =
    let handleOpens { RequiredOpens = opens } =
        match opens with
        | [] -> []
        | _ ->
            opens
            |> List.map (
                IdentListNode.make
                >> fun openName ->
                    OpenModuleOrNamespaceNode(openName, Range.Zero)
                >> Open.ModuleOrNamespace
                )
            |> OpenListNode
            |> ModuleDecl.OpenList
            |> List.singleton
    let build (schema: Schema) =
        let makePropInterfaceName = schema.Config.AttributeReturnTypes.Mapping
            
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
        let makeSimpleMembers propInterfaceName =
            InterfaceAttributeTypes.unsafeSimpleAttrPairs
            >> List.map (function
                | attrName, InterfaceAttributeType.Simple typ ->
                    BindingNode.makeSimple(attrName, typ, propInterfaceName)
                | attrName, InterfaceAttributeType.Object typ ->
                    BindingNode.makeObject(attrName, typ, propInterfaceName)
                | attrName, InterfaceAttributeType.NestedObject typ ->
                    BindingNode.makeNested(attrName, typ, propInterfaceName)
                | attrName, InterfaceAttributeType.ListToArray typ ->
                    BindingNode.makeListToArray(attrName, typ, propInterfaceName)
                | attrName, InterfaceAttributeType.ListToSeq typ ->
                    BindingNode.makeListToSeq(attrName, typ, propInterfaceName)
                | attrName, InterfaceAttributeType.Funcify typ ->
                    BindingNode.makeFunc(attrName, typ, propInterfaceName)
                | attrName, InterfaceAttributeType.CustomProp typ ->
                    BindingNode.makeCustomProp(attrName, typ, propInterfaceName)
                | attrName, InterfaceAttributeType.CustomPropObject typ ->
                    BindingNode.makeCustomPropObject(attrName, typ, propInterfaceName)
                | attrName, InterfaceAttributeType.KeyStringValueObjects typ ->
                    BindingNode.makeKeyValueObject(attrName, typ, propInterfaceName)
                | _ -> failwith "UNREACHABLE"
                >> MemberDefn.Member
                )
            
        [
            yield! handleOpens schema
            for interfaceDef in schema.Interfaces do
                let propInterfaceName = makePropInterfaceName interfaceDef.TypeName
                let makeSimpleMembers' = makeSimpleMembers propInterfaceName
                yield makePropInterfaceType propInterfaceName
                match interfaceDef with
                | InterfaceType.Simple(typName, attrs) ->
                    let members = makeSimpleMembers' attrs
                    yield
                        TypeNameNode.makeSimple(typName, attributes = [ "Erase" ])
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
                                    complexTyps |> List.partition (_.IsEnum >> not)
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
                                                    $"unbox(\"{attrName}\", {exprInfo.ParamName})"
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
                            | _ -> TypeDefnRegularNode.make (makeSimpleMembers' simpleAttrs)
                        |> TypeDefn.Regular
                        |> ModuleDecl.TypeDefn
        ] |> fun decls ->
        Oak([], [
            match schema with
            | { Namespace = Some value } ->
                ModuleOrNamespaceHeaderNode(
                    None,
                    None,
                    MultipleTextsNode.make "namespace",
                    None,
                    false,
                    Some(IdentListNode.make value),
                    Range.Zero
                    )
                |> Some
            | _ -> None
            |> fun header ->
                ModuleOrNamespaceNode(header, decls, Range.Zero)
        ], Range.Zero)
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
