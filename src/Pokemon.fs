module Pokemon

open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma
open Fulma.FontAwesome
open Fable.PowerPack
open Fable.PowerPack.Fetch

type Pokemon =
    { name : string
      weight : int }

type Model =
    { Loading : bool
      PokemonData : Option<Result<Pokemon, string>>
      PokemonId : int }

type Msg =
    | LoadPokemon
    | PokemonLoaded of Pokemon
    | LoadingFailed of string
    | IncreasePokemonId
    | DecreasePokemonId

let maxIdSoFar (pokemon : Map<int, Pokemon>) =
    pokemon
    |> Map.toList
    |> List.map fst
    |> List.max


let fetchPokemon pokemonId =
    promise {
        let url = (sprintf "https://pokeapi.co/api/v2/pokemon/%d/" pokemonId)

        let props =
            [ RequestProperties.Method HttpMethod.GET
              requestHeaders [ HttpRequestHeaders.Accept "application/json" ] ]
        return! fetchAs<Pokemon> url props
    }

let idSelector model dispatch =
    [ Level.level []
          [ Level.item [ Level.Item.HasTextCentered ]
                [ Button.button
                      [ Button.Color IsDanger

                        Button.Props
                            [ OnClick(fun _ -> dispatch DecreasePokemonId) ] ]
                      [ str "-" ] ]

            Level.item [ Level.Item.HasTextCentered ]
                [ str (sprintf "%d" model.PokemonId) ]

            Level.item [ Level.Item.HasTextCentered ]
                [ Button.button
                      [ Button.Color IsSuccess

                        Button.Props
                            [ OnClick(fun _ -> dispatch IncreasePokemonId) ] ]
                      [ str "+" ] ] ]

      Level.level []
          [ Level.item [ Level.Item.HasTextCentered ]
                [ Button.button
                      [ Button.IsActive(not model.Loading)
                        Button.Color IsPrimary
                        Button.Props [ OnClick(fun _ -> dispatch LoadPokemon) ] ]
                      [ str "Load Pokemon!" ] ] ] ]

let init _ =
    let model =
        { Loading = false
          PokemonData = None
          PokemonId =  25}
    model, Cmd.ofMsg LoadPokemon

let update msg model =
    match msg with
    | LoadPokemon ->
        { model with Loading = true
                     PokemonData = None },
        // Pick a different random number here between
        // 1 and 949
        Cmd.ofPromise fetchPokemon model.PokemonId PokemonLoaded
            (fun e -> LoadingFailed e.Message)
    | IncreasePokemonId ->
        { model with PokemonId = model.PokemonId + 1 }, Cmd.none
    | DecreasePokemonId ->
        { model with PokemonId = model.PokemonId - 1 }, Cmd.none

    | PokemonLoaded data ->
        { model with PokemonData = Some(Ok data)
                     Loading = false }, Cmd.none
    | LoadingFailed error ->
        { model with PokemonData = Some(Error error)
                     Loading = false }, Cmd.none


let view model dispatch =
    let content =
        if model.Loading then [ p [] [ str "Loooading!" ] ]
        else
            match model.PokemonData with
            | Some(Ok data) ->
                [ p [] [ str (sprintf "Name: %s" data.name) ]
                  p [] [ str (sprintf "Weight: %d" data.weight) ] ]
            | Some(Error message) ->
                [ p [] [ str "Oh noes: it went wrong! The error was:" ]
                  p [] [ str message ] ]
            | None -> [ p [] [ str "No Pokemon loaded." ] ]
    Container.container []
        [ Content.content [] (List.append content (idSelector model dispatch)) ]
