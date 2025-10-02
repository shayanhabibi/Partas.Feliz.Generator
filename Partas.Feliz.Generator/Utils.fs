module Partas.Feliz.Generator.Utils
open System
open Partas.Feliz.Generator.Spec
open Str

module String =
    let capitalize (str: string) =
        if str.Length <= 1 then
            str.ToUpper()
        else
            str.Substring(0,1).ToUpper()
            + str.Substring(1)
    let lowerFirst (str: string) =
        if str.Length <= 1 then
            str.ToLower()
        else
            str.Substring(0,1).ToLower()
            + str.Substring(1)
    let mangleReservedKeywords (text: string) =
        if reserved.Contains(text)
        then text + "'"
        else text
let mangleReservedKeywords = String.mangleReservedKeywords
