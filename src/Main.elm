port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Debounce exposing (Debounce)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Lazy exposing (lazy)
import Json.Decode as Decode
import Platform.Cmd exposing (Cmd)
import Task
import Utils exposing (..)


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

    -- Dark Mode
    , darkModeEnable : Bool

    -- Responsive
    , menuIsOpen : Bool
    }


type Msg
    = ToggleSubCategory String
    | ChangeCategory Category
    | FavoriteCategory
    | WithoutCategory
    | ToggleDarkMode
    | ToggleMenu
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
      , menuIsOpen = False

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
    div [ id "page" ]
        [ burger model
        , sidebar model
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


slugify : String -> Html Msg -> Html Msg
slugify slugText child =
    a [ slugText |> slug |> href ] [ child ]


categorySidebar : Category -> Html Msg
categorySidebar category =
    let
        subCategoryLinksCount =
            category.subCategories
                |> List.map (.values >> List.length)
                |> List.sum

        totalLinksCount =
            category.values
                |> List.length
                |> (+) subCategoryLinksCount
                |> String.fromInt

        title =
            category.name ++ " (" ++ totalLinksCount ++ ")"
    in
    slugify category.name <|
        div
            [ class "btn category__title", onClick (ChangeCategory category) ]
            [ text title ]


titleLink : Html Msg
titleLink =
    a
        [ id "page-title"
        , class "pointer"
        , target "_blank"
        , href "https://repl.it/@danielbom/SaveLinks"
        ]
        [ text "Save Links" ]


spaceBlock : Html Msg
spaceBlock =
    div [ class "empty-block" ] []


sidebar : Model -> Html Msg
sidebar model =
    let
        sidebarClass =
            if model.menuIsOpen then
                ""

            else
                "close"

        darkOrLightMode =
            if model.darkModeEnable then
                "🌔 Light Mode"

            else
                "🌒 Dark Mode"

        infoText =
            (model.allLinks |> List.length |> String.fromInt)
                ++ " links e "
                ++ (model.categories |> List.length |> String.fromInt)
                ++ " categorias"

        toggleDarkModeButton =
            div []
                [ div
                    [ class "pointer btn"
                    , onClick ToggleDarkMode
                    ]
                    [ text darkOrLightMode ]
                ]

        specialCategories =
            div []
                [ div
                    [ class "sidebar__subtitle" ]
                    [ text "Links" ]
                , div [ class "sidebar__group" ]
                    [ slugify "Sem categoria" <|
                        div
                            [ class "pointer btn", onClick WithoutCategory ]
                            [ text "Sem categoria" ]
                    , slugify "Favoritos" <|
                        div
                            [ class "pointer btn", onClick FavoriteCategory ]
                            [ text "Favoritos" ]
                    ]
                ]

        categoriesList =
            div []
                [ div
                    [ class "sidebar__subtitle" ]
                    [ text "Categorias" ]
                , div
                    [ id "category-add", class "pointer" ]
                    [ text "Adicionar categoria (+)" ]
                , div [ class "sidebar__group" ] <|
                    List.map categorySidebar model.categories
                ]

        plusSection =
            div [ id "plus-tools" ]
                [ div [ class "sidebar__subtitle" ] [ text "Main" ]
                , div [ class "sidebar__group" ]
                    [ div [ class "pointer btn" ] [ text "Rascunho" ]
                    , div [ class "pointer btn" ] [ text "Sair" ]
                    ]
                ]

        informations =
            div [ id "descriptions" ]
                [ text infoText ]
    in
    div
        [ id "sidebar", class sidebarClass ]
        [ titleLink
        , toggleDarkModeButton
        , specialCategories
        , categoriesList
        , plusSection
        , informations
        , spaceBlock
        ]


favicon : String -> String
favicon url =
    "https://www.google.com/s2/favicons?domain=" ++ optimizeFavicon url


linkView : Link -> Html Msg
linkView { title, url, favorite } =
    let
        icon =
            div [ class "link__icon" ]
                [ img [ alt title, src (favicon url) ] [] ]

        titleClass =
            if favorite then
                "link__title link__favorite"

            else
                "link__title"

        contentLink =
            div [ class titleClass ] [ text title ]
    in
    a [ target "_blank", class "link", href url ]
        [ icon, contentLink ]


linksView : List Link -> Html Msg
linksView currentLinks =
    div [] <| List.map linkView currentLinks


subCategoryView : SubCategory -> Html Msg
subCategoryView { name, values, show } =
    let
        subCategoryLinksCount =
            values |> List.length |> String.fromInt

        subCategoryName =
            name ++ " (" ++ subCategoryLinksCount ++ ")"

        subCategoryButton =
            button
                [ class "sub-category__title pointer btn"
                , onClick (ToggleSubCategory name)
                ]
                [ text subCategoryName ]

        subCategoryHeader =
            div
                [ class "sub-category" ]
                [ div
                    [ class "sub-category__icon" ]
                    [ div [ class "folder" ] [] ]
                , subCategoryButton
                ]

        subCategoryItems =
            div [ class "sub-category__items" ] <|
                if show then
                    List.map linkView values

                else
                    []
    in
    div
        [ class "sub-category__list" ]
        [ subCategoryHeader, subCategoryItems ]


subCategoriesView : List SubCategory -> Html Msg
subCategoriesView subCategories =
    div [ class "group__sub-category" ] <|
        List.map subCategoryView subCategories


content : Model -> Html Msg
content model =
    let
        contentClass =
            if model.menuIsOpen then
                "close"

            else
                ""

        searchBarClass =
            if model.searchLinksFound then
                "row input-text"

            else
                "row input-text input__error"

        searchBar =
            div
                [ class searchBarClass ]
                [ input
                    [ placeholder "Buscar"
                    , type_ "text"
                    , spellcheck False
                    , onInput UpdateSearch
                    , type_ "text"
                    , id "input-search"
                    ]
                    []
                ]

        inputAdd =
            div [ class "row input-search", id "input-add" ]
                [ input
                    [ placeholder "http:// ou https://"
                    , type_ "text"
                    , spellcheck False
                    ]
                    []
                ]

        searchResult =
            lazy linksView model.searchLinks

        currentCategoryTitle =
            div [ class "row", id "category-title" ]
                [ text model.currentTitle ]

        subCategoriesList =
            lazy subCategoriesView model.currentSubCategories

        baseLinksView =
            lazy linksView model.currentLinks
    in
    div [ id "content", class contentClass ]
        [ searchBar
        , inputAdd
        , searchResult
        , currentCategoryTitle
        , subCategoriesList
        , baseLinksView
        , spaceBlock
        ]


burger : Model -> Html Msg
burger model =
    let
        burgerClass =
            if model.menuIsOpen then
                "menu-btn open"

            else
                "menu-btn"

        burderMenu =
            div
                [ class burgerClass, onClick ToggleMenu ]
                [ div [ class "menu-btn__burger" ] [] ]
    in
    div [ id "header", class "row" ]
        [ titleLink
        , burderMenu
        ]



-- Update


tranformSearchLinkTitle : Link -> Link
tranformSearchLinkTitle link =
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

        searchFilter link =
            link.title
                |> String.toLower
                |> String.contains q

        searchLength =
            String.length model.search
    in
    if searchLength == 0 then
        []

    else if searchLength < 4 then
        model.searchLinks

    else
        model.allLinks
            |> List.filter searchFilter
            |> List.map tranformSearchLinkTitle


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
    let
        searchLengthIsEnough =
            String.length model.search >= 4
    in
    if searchLengthIsEnough then
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
                , menuIsOpen = False
              }
            , Cmd.none
            )

        FavoriteCategory ->
            ( { model
                | currentTitle = "Favoritos"
                , currentLinks = model.favorites
                , currentSubCategories = []
                , menuIsOpen = False
              }
            , Cmd.none
            )

        WithoutCategory ->
            ( { model
                | currentTitle = "Sem categoria"
                , currentLinks = model.withoutCategory
                , currentSubCategories = []
                , menuIsOpen = False
              }
            , Cmd.none
            )

        -- Others
        ToggleDarkMode ->
            ( { model | darkModeEnable = not model.darkModeEnable }
            , toggleDarkMode (not model.darkModeEnable)
            )

        ToggleMenu ->
            ( { model | menuIsOpen = not model.menuIsOpen }
            , Cmd.none
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
                    Task.perform
                        (\_ -> ChangeSearchList)
                        (Task.succeed s)

                ( debounce, cmd ) =
                    Debounce.update
                        debounceConfigSearch
                        (Debounce.takeLast changeSearchList)
                        deboundMsg
                        model.debounceSearch
            in
            ( { model | debounceSearch = debounce }, cmd )

        ChangeSearchList ->
            let
                newModel =
                    { model
                        | searchLinks = updateSearchedLinks model
                    }
            in
            ( { newModel
                | searchLinksFound = hasSearchLinks newModel
              }
            , Cmd.none
            )

        -- Shortcut: Search
        PressedKey "Escape" ->
            -- Blur the input if "ESC" was pressed
            let
                doBlur =
                    Task.attempt
                        (\_ -> NoOp)
                        (Dom.blur "input-search")
            in
            ( { model | searchOnFocus = False }, doBlur )

        PressedKey "q" ->
            -- Focus the input if "Shift + F" was pressed
            let
                ( focus, doFocus ) =
                    if model.searchOnFocus then
                        ( False, Cmd.none )

                    else
                        ( True
                        , Task.attempt
                            (\_ -> NoOp)
                            (Dom.focus "input-search")
                        )
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
