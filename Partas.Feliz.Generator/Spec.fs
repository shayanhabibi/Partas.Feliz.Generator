namespace Partas.Feliz.Generator

open System.Collections.Frozen

module Spec =
    /// <summary>
    /// Reserved keywords
    /// </summary>
    let reserved =
        [
          "abstract"; "and"; "as"; "assert"; "base"; "begin"; "class"; "default"
          "delegate"; "do"; "done"; "downcast"; "downto"; "elif"; "else"
          "end"; "exception"; "extern"; "false"; "finally"; "fixed"
          "fun"; "function"; "global"; "if"; "in"; "inherit"; "inline"
          "interface"; "internal"; "lazy"; "let"; "match"; "member"
          "module"; "mutable"; "namespace"; "new"; "null"; "of"; "open"
          "or"; "override"; "private"; "public"; "rec"; "return"
          "static"; "struct"; "then"; "to"; "true"; "try"; "type"
          "upcast"; "use"; "val"; "void"; "when"; "while" ;"with"; "yield"
          "const"
          // not actually reserved, but better not to obfuscate
          "not"; "select"
          // reserved because they are keywords in OCaml
          "asr"; "land"; "lor"; "lsl"; "lsr"; "lxor"; "mod"; "sig"
          // reserved for future expansion
          "break"; "checked"; "component"; "constraint"; "continue"; "event"
          "external"; "include"; "mixin"; "parallel"; "process"; "protected"
          "pure"; "sealed"; "tailcall"; "trait"; "virtual"
        ].ToFrozenSet()
[<AutoOpen>]
module Constants =
    [<Literal>]
    let number = "%MACRO%number"
    [<Literal>]
    let internal OBJECT = "%MACRO%createObj "
    [<Literal>]
    let internal NESTED_OBJECT = "%MACRO%nestedObj "
    [<Literal>]
    let internal LIST_ARRAY_TRANSFORM = "%MACRO%list->array "
    [<Literal>]
    let internal LIST_SEQ_TRANSFORM = "%MACRO%list->seq "
    [<Literal>]
    let internal FUNCIFY = "%MACRO%Funcify "
    [<Literal>]
    let numberArray = "%MACRO%number array"
    [<Literal>]
    let internal NUMBER = number
    [<Literal>]
    let internal NUMBER_ARRAY = numberArray
    [<Literal>]
    let internal XMLDOC = "%MACRO%xmlDoc "
    let object = (+) OBJECT
    let nestedObject = (+) NESTED_OBJECT
    let listToArray = (+) LIST_ARRAY_TRANSFORM
    let listToSeq = (+) LIST_SEQ_TRANSFORM
    let func = (+) FUNCIFY
    let doc = (+) XMLDOC

