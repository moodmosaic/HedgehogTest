open System.Xml
open System.Xml.Linq
open Fare
open Hedgehog

module M =
    let baseDocText = """<?xml version="1.0" encoding="UTF-8"?><root />"""
        
    type XmlTree =
        | NodeName of string 
        | Container of string * XmlTree list
        
    let nodeNames =
        [ "myNode"
          "myOtherNode"
          "someDifferentNode" ]
          
    let tree =
        let rec tree' s =
            match s with
            | 0 -> nodeNames |> Gen.item |> Gen.map NodeName
            | n when n > 0 ->
                let subtrees =
                    Gen.sized (fun s ->
                        Gen.list (Range.linear 0 (int (sqrt (float s)))) (tree' (n / 2)))
                
                Gen.choice
                    [ nodeNames |> Gen.item |> Gen.map NodeName
                      (Gen.item nodeNames, subtrees) ||> Gen.map2 (fun name contents -> Container (name, contents)) ]
            | _ -> invalidArg "s" "Size must be positive"
        
        Gen.sized tree'
        
    let treeToXDoc xmlTree =
        let rec inner currentNode children =
            let childMatch child =
                match child with
                | NodeName name -> XElement (XName.Get name)
                | Container (name, content) ->
                    let element = XElement (XName.Get name)
                    inner element content
            currentNode.Add (children |> List.map childMatch |> List.toArray)
            currentNode
        
        match xmlTree with
        | NodeName name -> XDocument (XElement (XName.Get name))
        | Container (name, content) ->
            let doc = XDocument (XElement (XName.Get name))
            inner doc.Root content |> ignore
            doc

[<EntryPoint>]
let main _ =
    let filterInvalidChars (input : string) =
        input
        |> Seq.filter (fun c -> XmlConvert.IsXmlChar c)
        |> Seq.map string
        |> String.concat ""
    
    let (|ValidXml|InvalidXml|) str =
        let filtered = filterInvalidChars str
        if String.length filtered = String.length str then
            ValidXml str
        else
            InvalidXml filtered

    let addEnhancement (doc: XDocument) (input: string) =
        match input with
        | ValidXml input ->
            if not (doc.Root.Elements(XName.Get "Enhancement") |> Seq.exists (fun x -> x.Value = input)) then
                doc.Root.Add(XElement(XName.Get "Enhancement", input))
        | InvalidXml input ->
            if not (doc.Root.Elements(XName.Get "Error") |> Seq.exists (fun x -> x.Value = input)) then
                doc.Root.Add(XElement(XName.Get "Error", input))
        doc
        
    let baseDoc = "<root />"
    
    let ``add enhancement must be idempotent`` input =
        let xml1 = XDocument.Parse baseDoc
        let xml2 = XDocument.Parse baseDoc
        (addEnhancement xml1 input).ToString() =
            (addEnhancement (addEnhancement xml2 input) input).ToString()

    Property.print <| property {
        let! s = Gen.string (Range.linear 0 10) Gen.ascii
        return ``add enhancement must be idempotent`` s 
    }
    
    Property.print <| property {
        let! data = Gen.string (Range.linear 0 10) Gen.ascii
        let! xmlDoc = M.tree
        return 
            (addEnhancement (M.treeToXDoc xmlDoc) data).ToString() = 
                (addEnhancement (addEnhancement (M.treeToXDoc xmlDoc) data) data).ToString()
    }
    
    Property.print <| property {
        let! data = Gen.string (Range.linear 0 10) Gen.ascii
        let! xmlDoc = M.tree
        return 
            Seq.length ((M.treeToXDoc xmlDoc).DescendantNodes()) = 
                Seq.length ((addEnhancement (M.treeToXDoc xmlDoc) data).DescendantNodes())
    }

     












//    let fromRegex (pattern: string) : Gen<string> =
//        Gen.sized (fun size ->
//            let xeger = Xeger pattern
//            Gen.item [ for _ in 1..size -> xeger.Generate() ]
//        )
    
//    property {
//        let! s = fromRegex @"^\[\<([A-Z][a-zA-Z0-9]*)*\>\]$"
////        where (s.Length = 4 || s.Length > 5)
//        printfn "%s" s
//        return true
//    } |> Property.print' 10<tests>
// 
    
//    let rnd = System.Random ()
//    for _ in 1..10 do
//        (Xeger (@"^\[\<([A-Z][a-zA-Z0-9]*)*\>\]$", rnd)).Generate() |> printfn "%s"
 
    0
