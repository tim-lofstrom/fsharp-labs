open System.Net.Http
open System.Text.Json

type Post =
    { userId: int
      id: int
      title: string
      body: string }

let fetchData (url: string) : string =
    use httpClient = new HttpClient()
    httpClient.GetStringAsync(url).Result

[<EntryPoint>]
let main argv =

    let ids = [| 1; 2; 3 |]
    let baseUrl = "https://jsonplaceholder.typicode.com/posts/"
    let urls = ids |> Array.map (fun id -> baseUrl + string id)

    let results =
        urls |> Array.map (fun url -> fetchData url |> JsonSerializer.Deserialize<Post>)

    results
    |> Array.iter (fun post ->
        match post with
        | { userId = 1 } -> printfn $"Post from User ID: 1 {post.title}"
        | _ -> printfn $"Post from user {post.title}")

    0
