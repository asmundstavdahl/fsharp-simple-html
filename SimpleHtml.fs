module SimpleHtml

type Attribute = string * string
let node tagName attributes children =
    let attributeStrings =
        attributes
        |> List.map (fun (name, value) -> sprintf "%s=\"%s\"" name value)
    let printer =
        match attributes with
        | [] -> sprintf "<%s%s>%s</%s>"
        | _ -> sprintf "<%s %s>%s</%s>"
    let childrenHtml =
        String.concat "\n" ("" :: children)
    let attributesHtml =
        String.concat " " attributeStrings
    printer
        <| tagName
        <| attributesHtml
        <| childrenHtml
        <| tagName

let onload js: Attribute = ("onload", js)

let html a c = node "html" a c
let head a c = node "head" a c
let body a c = node "body" a c
let div a c = node "div" a c
let span a c = node "span" a c
let h1 a c = node "h1" a c
let h2 a c = node "h2" a c
let h3 a c = node "h3" a c
let h4 a c = node "h4" a c
let br = node "br" [] []
let hr = node "hr" [] []

let output =
    html [] [
        head [] []
        body [onload "init"] [
            h1 [] ["Test av Åsmunds super-HTML-bibliotek for F#"]
            div [("style", "background-color: lightblue;")] [
                "Voluptatem nisi eum velit repellendus omnis. Similique eum culpa blanditiis corporis voluptatem. Ipsum temporibus explicabo voluptates illo tenetur."
                br
                "Harum aliquid culpa repudiandae. Ab esse perferendis consequuntur. Minus animi voluptatem dolor placeat aut praesentium deleniti. Et est et occaecati illum voluptas. Dolorem assumenda et tempora quia."
                br
                "Enim quod perspiciatis quo aut. Dolor consequatur quia est eos nobis omnis sed dolores. Alias doloribus consequatur vitae est itaque."
                br
                "Omnis sed fugit quod voluptas. Eaque omnis vero occaecati minima aut voluptas. Nisi vel nam nam atque alias excepturi dolor molestiae. Vero soluta ut nulla occaecati ad."
                br
                "Repellendus ex accusamus dolores odio molestiae. Dolorem non placeat voluptate et. Et neque repellat reiciendis deserunt. Perferendis voluptatibus eum aut nihil sapiente quae eius. Voluptatibus minus rerum rerum est minima delectus accusamus in."
            ]
        ]
    ]

printfn "%s" output
