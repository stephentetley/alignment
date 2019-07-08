// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

module SLAlignment.Old.Common

/// The Excel Type Provider seems to read a trailing null row.
/// This dictionary and procedure provide a skeleton to get round this.
type ExcelProviderHelperDict<'table, 'row> = 
    { GetRows : 'table -> seq<'row>
      NotNullProc : 'row -> bool }

      
let excelTableGetRows (dict:ExcelProviderHelperDict<'table,'row>) (table:'table) : seq<'row> = 
    let allrows = dict.GetRows table
    allrows |> Seq.filter dict.NotNullProc 