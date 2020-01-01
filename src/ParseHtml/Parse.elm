module ParseHtml.Parse exposing (do)

import Char
import Dict exposing (Dict)
import Html exposing (Attribute, Html, div, node, text)
import Html.Attributes as Attr
import List exposing (concat, filter, foldl, map, reverse, sortBy)
import List.Extra exposing (splitWhen)
import ParseHtml.HtmlEntities exposing (htmlEntities)
import String exposing (dropLeft, dropRight, indices, join, left, length, right, slice, split, startsWith, words)


type alias Tag =
    { node : String
    , attrs : List ( String, String )
    , beg : Int
    , end : Int
    , contentBeg : Int
    , contentEnd : Int
    , selfClosing : Bool
    , children : Children
    }


type Child
    = Child Tag


type alias Children =
    List Child


type alias Tags =
    List Tag


allowedAttributes =
    [ "class", "href", "src" ] --all other attributes are ignored


skipThese =
    [ "font" ] --these tags are ignored


deleteThese =
    [ "style" ] --these tags, together with their content, are removed - rendering them in Elm is a pain


do : String -> Html msg
do txt =
    case removeNothings (map (toHtmlTag txt) (tagsIn txt)) of
        [] ->
            div [] []

        [ x ] ->
            x

        xs ->
            div [] xs


toHtmlTag : String -> Tag -> Maybe (Html msg)
toHtmlTag txt tag =
    let
        generator a =
            node a.node

        fixedContent =
            fixEntities rawContent

        rawContent =
            contentTxt txt tag

        recurseOn (Child a) =
            toHtmlTag txt a

        mapper ( a, b ) =
            Attr.attribute a b

        attributes =
            List.map mapper tag.attrs
    in
    case tag.node of
        "" ->
            Just (text fixedContent)

        _ ->
            case List.member tag.node deleteThese of
                True ->
                    Nothing

                False ->
                    case List.member tag.node skipThese of
                        True ->
                            case tag.children of
                                [] ->
                                    Nothing

                                children ->
                                    Just <| div [] <| removeNothings (map recurseOn children)

                        False ->
                            case tag.selfClosing of
                                True ->
                                    Just <| generator tag attributes []

                                False ->
                                    case tag.children of
                                        [] ->
                                            Just <| generator tag attributes [ text fixedContent ]

                                        children ->
                                            Just <| generator tag attributes <| removeNothings (map recurseOn children)



---------


addContentEnd : Tags -> String -> Tag -> Tags
addContentEnd tags txt tag =
    let
        closer =
            left 1 tag.node == "/"

        ( contentEnd, end ) =
            contentEnds (length txt) tag tags
    in
    case tag.selfClosing of
        True ->
            [ tag ]

        False ->
            case closer of
                True ->
                    []

                --remove </div> tags
                False ->
                    [ { tag | contentEnd = contentEnd, end = end } ]


addTexts : String -> Tags -> Tags
addTexts txt tags =
    let
        idekell =
            addTextsRecursion [] <| ( -1, -1 ) :: pairs ++ [ ( txtLength, txtLength ) ]

        pairs =
            sortBy first <| map toPair tags

        toPair a =
            ( a.beg, a.end )

        txtLength =
            length txt
    in
    sortBy .beg <| map filler idekell ++ map addTextsToChildren tags


addTextsToChildren : Tag -> Tag
addTextsToChildren tag =
    let
        childBeg (Child a) =
            a.beg

        idekell =
            addTextsRecursion [] <| ( tBeg - 1, tBeg - 1 ) :: pairs ++ [ ( tEnd, tEnd ) ]

        pairs =
            sortBy first <| map toPair tag.children

        tBeg =
            tag.contentBeg

        tEnd =
            tag.contentEnd

        toPair (Child a) =
            ( a.beg, a.end )

        treatChild (Child a) =
            Child (addTextsToChildren a)
    in
    { tag | children = sortBy childBeg <| map (Child << filler) idekell ++ map treatChild tag.children }


addTextsRecursion : List ( Int, Int ) -> List ( Int, Int ) -> List ( Int, Int )
addTextsRecursion output toCheck =
    let
        fillGap ( a, b ) ( c, d ) =
            case b + 1 < c of
                True ->
                    [ ( b + 1, c ) ]

                False ->
                    []
    in
    case toCheck of
        x :: y :: zs ->
            addTextsRecursion (output ++ fillGap x y) (y :: zs)

        _ ->
            output


char : Int -> String
char code =
    --decimal Unicode
    String.fromChar <| Char.fromCode code


childOf : Tag -> Tag -> Bool
childOf parent maybeChild =
    parent.contentEnd > maybeChild.beg && parent.beg < maybeChild.beg


closedBetween : Int -> Int -> String -> Tags -> Bool
closedBetween a b openingTag tags =
    numTagsBetween a b openingTag tags
        == numTagsBetween a b ("/" ++ openingTag) tags


contentEnds : Int -> Tag -> Tags -> ( Int, Int )
contentEnds txtLength tag tags =
    let
        tagType =
            tag.node

        closer a =
            (a.node == "/" ++ tag.node) && (a.beg > tag.beg)

        closers =
            filter closer tags
    in
    contentEndsRecursion txtLength tag closers tags


contentEndsRecursion : Int -> Tag -> Tags -> Tags -> ( Int, Int )
contentEndsRecursion txtLength tag remainingClosers tags =
    case remainingClosers of
        x :: xs ->
            case closedBetween tag.beg x.beg tag.node tags of
                True ->
                    ( x.beg, x.end )

                False ->
                    contentEndsRecursion txtLength tag xs tags

        [] ->
            ( txtLength - 1, txtLength - 1 )


contentTxt : String -> Tag -> String
contentTxt txt tag =
    slice tag.contentBeg tag.contentEnd txt


dequote : String -> String
dequote txt =
    let
        leftOff =
            case left 1 txt == "\"" of
                True ->
                    dropLeft 1

                False ->
                    identity

        rightOff =
            case right 1 txt == "\"" of
                True ->
                    dropRight 1

                False ->
                    identity
    in
    (leftOff << rightOff) txt


filler : ( Int, Int ) -> Tag
filler ( start, end ) =
    { node = ""
    , attrs = []
    , beg = start
    , end = end
    , contentBeg = start
    , contentEnd = end
    , selfClosing = False
    , children = []
    }


first ( a, b ) =
    a


fixEntity ( entity, code ) txt =
    join (char code) <| split entity txt


fixEntities txt =
    let
        fix =
            foldl (<<) identity <| map fixEntity htmlEntities
    in
    fix txt


grabTags : String -> Tags
grabTags txt =
    -- find <a href="xxx"> type strings
    -- turn them into list of (""a",[("href","xxx")])
    let
        begs =
            tagBegs txt

        ends =
            tagEnds txt

        greaterThan a ( b, c ) =
            b > a

        end a =
            filter (greaterThan a) ends

        makeTag a =
            case end a of
                ( b, c ) :: _ ->
                    [ { node = ""
                      , attrs = []
                      , beg = a
                      , end = b
                      , contentBeg = b + 1
                      , contentEnd = 0
                      , selfClosing = c
                      , children = []
                      }
                    ]

                [] ->
                    []
    in
    concat <| map makeTag begs


identify : String -> Tag -> Tags
identify txt tag =
    let
        tagChunks =
            words <| slice (tag.beg + 1) tag.end txt

        toAttr x y =
            case startsWith x y of
                True ->
                    [ ( x, dequote <| dropLeft (length x + 1) y ) ]

                False ->
                    []

        checkers =
            map toAttr allowedAttributes

        reverseMap xs f =
            concat <| map f xs

        attrs xs =
            concat <| map (reverseMap xs) checkers

        checkSelfClose a =
            case a.node of
                "br" ->
                    { a | selfClosing = True }

                "img" ->
                    { a | selfClosing = True }

                _ ->
                    a
    in
    case tagChunks of
        x :: xs ->
            [ checkSelfClose { tag | node = x, attrs = attrs xs } ]

        [] ->
            []


matryoshka : Tags -> Tags
matryoshka tags =
    matryoshkaRecursion [] (reverse tags)


matryoshkaRecursion : Tags -> Tags -> Tags
matryoshkaRecursion output toCheck =
    let
        isChild a b =
            childOf b a

        addChildTo a b =
            { a | children = Child b :: a.children }
    in
    case toCheck of
        x :: xs ->
            case splitWhen (isChild x) xs of
                Just ( ys, vs ) ->
                    case vs of
                        z :: zs ->
                            matryoshkaRecursion output <| ys ++ [ addChildTo z x ] ++ zs

                        [] ->
                            matryoshkaRecursion (x :: output) ys

                Nothing ->
                    matryoshkaRecursion (x :: output) xs

        [] ->
            output


numTagsBetween : Int -> Int -> String -> Tags -> Int
numTagsBetween a b tagType tags =
    let
        thisType x =
            x.node == tagType
    in
    List.length <| tagsBetween a b <| filter thisType tags


removeNothings xs =
    let
        remover x =
            case x of
                Nothing ->
                    []

                Just y ->
                    [ y ]
    in
    List.concat <| List.map remover xs


tagBegs : String -> List Int
tagBegs txt =
    indices "<" txt


tagEnds : String -> List ( Int, Bool )
tagEnds txt =
    let
        base =
            indices ">" txt

        selfCloser a =
            slice (a - 1) a txt == "/"

        addInfo a =
            ( a, selfCloser a )
    in
    map addInfo base


tagsBetween : Int -> Int -> Tags -> Tags
tagsBetween a b tags =
    let
        after x =
            filter (\y -> y.beg > x)

        before x =
            filter (\y -> y.beg < x)
    in
    after a <| before b tags


tagsIn : String -> Tags
tagsIn txt =
    let
        rawTags =
            grabTags txt

        identified =
            apply identify rawTags

        apply f xs =
            concat <| map (f txt) xs
    in
    addTexts txt <|
        matryoshka <|
            apply (addContentEnd identified) identified
