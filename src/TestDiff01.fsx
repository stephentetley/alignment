#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

open System.Text.RegularExpressions

#load "Alignment/Common.fs"
#load @"Alignment/OrderedList.fs"
open Alignment.Common
open Alignment.OrderedList


type UploadTable = 
    ExcelFile< @"G:\work\Projects\rtu\Erskines\alignment\EDMS-final-docs-uploaded-june-2018.xlsx",
               SheetName = "Sheet1",
               ForceString = true >

/// Note - we cannot implement interfaces (e.g. 'System.IComparable') for 
/// type abbreviations.
type UploadRow = UploadTable.Row


/// "ALBERT_WTW Erskine Battery Asset Replacement Install Sheet"
let normalizeTitle (str:string) : string = 
    let rest = str.IndexOf(" Erskine") - 1
    if rest > 0 then 
        // printfn "normalizeTitle: '%s'" (str.[0 .. rest].Replace('_', ' '))
        str.[0 .. rest].Replace('_', ' ')
    else
        str

let normalizeDirName (str:string) : string = 
    str.Trim().Replace('_', ' ')
    

let uploadKey (row:UploadRow) : string = normalizeTitle <| row.Title
let directoryKey (name:string) : string = normalizeDirName name



let uploadTableDict : ExcelProviderHelperDict<UploadTable, UploadRow> = 
    { GetRows     = fun sheet -> sheet.Data 
      NotNullProc = fun row -> match row.GetValue(0) with | null -> false | _ -> true }


let getSiteRows () : UploadRow list = 
    excelTableGetRows uploadTableDict (new UploadTable()) 
        |> Seq.toList 
        



let getDirectoryName (line:string) : option<string> = 
    let regMatch = Regex.Match(line, @"^[d]\-{4}[ 0-9/:]*(?<dirname>([A-Z][A-Z0-9 _]*))")
    if regMatch.Success then 
        Some <| regMatch.Groups.["dirname"].Value.Trim()
    else 
        None

let getDirectoryNames (listingPath:string) : string list = 
    let text:string list = System.IO.File.ReadAllLines(listingPath) |> Array.toList
    text |> List.map getDirectoryName |> List.choose id


let getUploads () : OrderedList<string,UploadRow> = 
    getSiteRows () 
        |> OrderedList.ofList uploadKey

let getDirectories () : OrderedList<string,string> = 
    getDirectoryNames @"G:\work\Projects\rtu\Erskines\alignment\dir.txt"
        |> OrderedList.ofList directoryKey


let test01 () = 
    differenceL (getDirectories ()) (getUploads ())
        |> toList
        |> List.iter (printfn "%s") 





