port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Debounce exposing (Debounce)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Decode
import Platform.Cmd exposing (Cmd)
import Task


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = subscriptions
        }



-- Init


type alias Link =
    { url : String
    , title : String
    , favorite : Bool
    , category : Maybe String
    , subCategory : Maybe String
    }


type alias SubCategory =
    { name : String
    , values : List Link
    , show : Bool
    }


type alias Category =
    { name : String
    , values : List Link
    , subCategories : List SubCategory
    , selected : Bool
    }


type alias Model =
    { -- Fixed values
      categories : List Category
    , favorites : List Link
    , withoutCategory : List Link
    , allLinks : List Link

    -- Current Values
    , currentLinks : List Link
    , currentSubCategories : List SubCategory
    , currentTitle : String

    -- Search
    , searchOnFocus : Bool
    , search : String
    , searchLinks : List Link
    , searchLinksFound : Bool
    , debounceSearch : Debounce String
    , darkModeEnable : Bool
    }


type Msg
    = ToggleSubCategory String
    | ChangeCategory Category
    | FavoriteCategory
    | WithoutCategory
    | ToggleDarkMode
      -- Search with debounce
    | UpdateSearch String
    | DebounceSearch Debounce.Msg
    | ChangeSearchList
      -- Shortcuts
    | PressedKey String
    | NoOp


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        defaultCategory =
            { name = "Sem categoria"
            , values = flags.withoutCategory
            , subCategories = []
            , selected = True
            }

        currentCategory =
            List.filter (\c -> c.name == flags.state.category) flags.categories
                |> List.head
                |> Maybe.withDefault defaultCategory
    in
    ( { -- fixed values
        categories = flags.categories
      , favorites = flags.favorites
      , withoutCategory = flags.withoutCategory
      , allLinks = flags.allLinks

      -- current values
      , currentLinks = currentCategory.values
      , currentSubCategories = currentCategory.subCategories
      , currentTitle = currentCategory.name
      , darkModeEnable = flags.state.darkModeEnable

      -- search
      , searchOnFocus = False
      , search = ""
      , debounceSearch = Debounce.init
      , searchLinks = []
      , searchLinksFound = True
      }
    , toggleDarkMode flags.state.darkModeEnable
    )



-- View


view : Model -> Html Msg
view model =
    div []
        [ sidebar model
        , content model
        ]


slug : String -> String
slug text =
    let
        toString ch =
            String.cons ch ""

        pairReplace ( chReplaces, letters ) currText =
            String.foldl (\ch -> String.replace (toString ch) chReplaces)
                currText
                letters

        -- https://stackoverflow.com/questions/990904/remove-accents-diacritics-in-a-string-in-javascript
        normalize initialText =
            List.foldl pairReplace
                initialText
                [ ( "a", "áàâãå" )
                , ( "e", "éèẽê" )
                , ( "i", "ìíĩî" )
                , ( "o", "óòõô" )
                , ( "u", "úùũû" )
                , ( "c", "ç" )
                , ( "n", "ñ" )
                , ( "y", "ýÿ" )
                , ( "-", " " )
                ]
    in
    text
        |> String.toLower
        |> normalize
        |> (++) "#"


slugify : String -> List (Html Msg) -> Html Msg
slugify slugText children =
    a [ slugText |> slug |> href ] children


categorySidebar : Category -> Html Msg
categorySidebar category =
    let
        categoryClass =
            if category.selected then
                "btn category__select"

            else
                "btn"

        subCategoryCountLinks =
            List.sum <| List.map (\sc -> List.length sc.values) category.subCategories

        valuesLengthStr =
            String.fromInt <| List.length category.values + subCategoryCountLinks

        title =
            category.name ++ " (" ++ valuesLengthStr ++ ")"
    in
    slugify category.name
        [ div [ class categoryClass, onClick (ChangeCategory category) ]
            [ text title ]
        ]


sidebar : Model -> Html Msg
sidebar model =
    let
        darkOrLightMode =
            if model.darkModeEnable then
                "🌔 Light Mode"

            else
                "🌒 Dark Mode"

        infoText =
            (String.fromInt <| List.length model.allLinks)
                ++ " links e "
                ++ (String.fromInt <| List.length model.categories)
                ++ " categorias"
    in
    node "sidebar"
        [ class "sidebar" ]
        [ a
            [ class "sidebar__item sidebar__title"
            , target "_blank"
            , href "https://repl.it/@danielbom/SaveLinks"
            ]
            [ text "Save Links" ]
        , div [ class "sidebar__item" ]
            [ div
                [ class "btn"
                , onClick ToggleDarkMode
                ]
                [ text darkOrLightMode ]
            ]
        , div [ class "sidebar__item sidebar__links" ]
            [ div [ class "sidebar__subtitle" ]
                [ text "Links" ]
            , slugify "Sem categoria"
                [ div
                    [ class "btn", onClick WithoutCategory ]
                    [ text "Sem categoria" ]
                ]
            , slugify "Favoritos"
                [ div
                    [ class "btn", onClick FavoriteCategory ]
                    [ text "Favoritos" ]
                ]
            ]
        , div [ class "sidebar__item sidebar__categories" ]
            [ div [ class "sidebar__subtitle" ]
                [ text "Categorias" ]
            , div [ class "sidebar__category-add btn" ]
                [ text "Adicionar categoria (+)" ]
            , div [ class "group__category" ] <|
                List.map categorySidebar model.categories
            ]
        , div [ class "sidebar__item sidebar__plus" ]
            [ div [ class "sidebar__subtitle" ]
                [ text "Main" ]
            , div [ class "btn" ]
                [ text "Rascunho" ]
            , div [ class "btn" ]
                [ text "Sair" ]
            ]
        , div [ class "sidebar__item sidebar__describe" ]
            [ div []
                [ text infoText ]
            ]
        ]


linkView : Link -> Html Msg
linkView { title, url, favorite } =
    let
        titleClass =
            if favorite then
                "link__title link__favorite"

            else
                "link__title"
    in
    a [ target "_blank", class "link", href url ]
        [ div [ class "link__icon" ]
            [ img
                [ alt title
                , src ("https://www.google.com/s2/favicons?domain=" ++ url)
                ]
                []
            ]
        , div [ class titleClass ] [ text title ]
        ]


linksView : List Link -> String -> Html Msg
linksView currentLinks className =
    div [ class className ]
        (List.map linkView currentLinks)


subCategoryView : SubCategory -> Html Msg
subCategoryView { name, values, show } =
    section [ class "sub-category__list" ]
        [ div [ class "sub-category" ]
            [ div [ class "sub-category__icon" ] []
            , button
                [ class "sub-category__title"
                , onClick (ToggleSubCategory name)
                ]
                [ text (name ++ " (" ++ (List.length values |> String.fromInt) ++ ")")
                ]
            ]
        , div [ class "sub-category__items" ] <|
            if show then
                List.map linkView values

            else
                []
        ]


subCategoriesView : List SubCategory -> Html Msg
subCategoriesView subCategories =
    div [ class "group__sub-category" ]
        (List.map subCategoryView subCategories)


searchErrorClass : Model -> String
searchErrorClass model =
    if model.searchLinksFound then
        ""

    else
        " input__error"


content : Model -> Html Msg
content model =
    main_ []
        [ div
            [ class ("row-block input__search" ++ searchErrorClass model) ]
            [ input
                [ placeholder "Buscar"
                , type_ "text"
                , spellcheck False
                , onInput UpdateSearch
                , type_ "text"
                , list "links-title"
                , id "input-search"
                ]
                []
            ]
        , div [ class "row-block input__url" ]
            [ input [ placeholder "http:// ou https://", type_ "text" ] []
            ]
        , lazy2 linksView model.searchLinks "group__searched"
        , div [ class "row-block main__category" ]
            [ text model.currentTitle
            ]
        , lazy subCategoriesView model.currentSubCategories
        , lazy2 linksView model.currentLinks "group__link"
        , div [ class "row-block" ] []
        ]



-- Update


tranformSearchLink : Link -> Link
tranformSearchLink link =
    let
        locationIdentifier =
            case ( link.category, link.subCategory ) of
                ( Nothing, Nothing ) ->
                    ""

                ( Just c, Nothing ) ->
                    "[" ++ c ++ "] "

                ( Nothing, Just sc ) ->
                    "[" ++ sc ++ "] "

                ( Just c, Just sc ) ->
                    "[" ++ c ++ " | " ++ sc ++ "] "
    in
    { link | title = locationIdentifier ++ link.title }


updateSearchedLinks : Model -> List Link
updateSearchedLinks model =
    let
        q =
            String.toLower model.search

        searchLength =
            String.length model.search
    in
    if searchLength == 0 then
        []

    else if searchLength < 4 then
        model.searchLinks

    else
        List.map tranformSearchLink <|
            List.filter
                (\link -> String.toLower link.title |> String.contains q)
                model.allLinks


toggleSubCategory : String -> SubCategory -> SubCategory
toggleSubCategory subCategoryName subCategory =
    { subCategory
        | show =
            if subCategory.name == subCategoryName then
                not subCategory.show

            else
                subCategory.show
    }


hasSearchLinks : Model -> Bool
hasSearchLinks model =
    if String.length model.search >= 4 then
        List.length model.searchLinks > 0

    else
        True


debounceConfigSearch : Debounce.Config Msg
debounceConfigSearch =
    { strategy = Debounce.later 1000
    , transform = DebounceSearch
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleSubCategory subCategoryName ->
            ( { model
                | currentSubCategories =
                    List.map
                        (toggleSubCategory subCategoryName)
                        model.currentSubCategories
              }
            , Cmd.none
            )

        -- Change Category
        ChangeCategory category ->
            ( { model
                | currentTitle = category.name
                , currentLinks = category.values
                , currentSubCategories = category.subCategories
              }
            , Cmd.none
            )

        FavoriteCategory ->
            ( { model
                | currentTitle = "Favoritos"
                , currentLinks = model.favorites
                , currentSubCategories = []
              }
            , Cmd.none
            )

        WithoutCategory ->
            ( { model
                | currentTitle = "Sem categoria"
                , currentLinks = model.withoutCategory
                , currentSubCategories = []
              }
            , Cmd.none
            )

        ToggleDarkMode ->
            ( { model | darkModeEnable = not model.darkModeEnable }
            , toggleDarkMode (not model.darkModeEnable)
            )

        -- Search
        UpdateSearch search ->
            let
                ( debounce, cmd ) =
                    Debounce.push
                        debounceConfigSearch
                        search
                        model.debounceSearch
            in
            ( { model
                | search = search
                , searchLinksFound = True
                , debounceSearch = debounce
              }
            , cmd
            )

        DebounceSearch deboundMsg ->
            let
                changeSearchList s =
                    Task.perform (\_ -> ChangeSearchList) (Task.succeed s)

                ( debounce, cmd ) =
                    Debounce.update
                        debounceConfigSearch
                        (Debounce.takeLast changeSearchList)
                        deboundMsg
                        model.debounceSearch
            in
            ( { model | debounceSearch = debounce }
            , cmd
            )

        ChangeSearchList ->
            let
                newModel =
                    { model | searchLinks = updateSearchedLinks model }
            in
            ( { newModel | searchLinksFound = hasSearchLinks newModel }
            , Cmd.none
            )

        -- Shortcut: Search
        PressedKey "Escape" ->
            -- Blur the input if "ESC" was pressed
            let
                doBlur =
                    Task.attempt (\_ -> NoOp) (Dom.blur "input-search")
            in
            ( { model | searchOnFocus = False }, doBlur )

        PressedKey "F" ->
            -- Focus the input if "Shift + F" was pressed
            let
                ( focus, doFocus ) =
                    if model.searchOnFocus then
                        ( False, Cmd.none )

                    else
                        ( True, Task.attempt (\_ -> NoOp) (Dom.focus "input-search") )
            in
            ( { model | searchOnFocus = focus }, doFocus )

        PressedKey _ ->
            update NoOp model

        NoOp ->
            ( model, Cmd.none )


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
    let
        ( newModel, cmds ) =
            update msg oldModel
    in
    ( newModel
    , Cmd.batch
        [ setStorage
            { category = newModel.currentTitle
            , darkModeEnable = newModel.darkModeEnable
            }
        , cmds
        ]
    )



-- Subscriptions


msgDecoder : Sub Msg
msgDecoder =
    Decode.field "key" Decode.string
        |> Decode.map PressedKey
        |> Browser.Events.onKeyDown


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ msgDecoder ]



-- Flags


type alias State =
    { category : String
    , darkModeEnable : Bool
    }


type alias Flags =
    { categories : List Category
    , allLinks : List Link
    , favorites : List Link
    , withoutCategory : List Link
    , state : State
    }


port setStorage : State -> Cmd msg


port toggleDarkMode : Bool -> Cmd msg
