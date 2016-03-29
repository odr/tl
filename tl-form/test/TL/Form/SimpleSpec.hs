{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE PolyKinds #-}
module TL.Form.SimpleSpec where

import Test.Hspec
import TL.Form.Simple
import TL.Form.Types
import Data.Tagged
import Lucid
import Data.Text(Text)
import qualified Data.Text as T
import GHC.TypeLits(Symbol, Nat)

type TRS  (sid::[Nat]) (rs :: [(Symbol, HtmlTag)]) v
    = Tagged '(Simple, sid, rs) (Maybe v)

spec :: Spec
spec = describe "Simple rendering" $ do
    describe "For output-only values" $ do
        it "render Int" $ do
            renderText (toHtml (Tagged 1 :: Tagged Simple Int))
                `shouldBe` "1"
        it "render Integer" $ do
            renderText (toHtml (Tagged 12345678901234567890 :: Tagged Simple Integer))
                `shouldBe` "12345678901234567890"
        it "render Float" $ do
            renderText (toHtml (Tagged 1 :: Tagged Simple Float))
                `shouldBe` "1.0"
        it "render Double" $ do
            renderText (toHtml (Tagged 1 :: Tagged Simple Double))
                `shouldBe` "1.0"
        it "render String" $ do
            renderText (toHtml (Tagged "русский язык" :: Tagged Simple String))
                `shouldBe` "русский язык"
        it "render Text" $ do
            renderText (toHtml (Tagged "עברית" :: Tagged Simple Text))
                `shouldBe` "עברית"
        context "with Maybe a" $ do
            it "render having value" $ do
                renderText (toHtml (Tagged (Just 1) :: Tagged Simple (Maybe Int)))
                    `shouldBe` "1"
            it "render having no value" $ do
                renderText (toHtml (Tagged Nothing :: Tagged Simple (Maybe Text)))
                    `shouldBe` ""
        context "when rendering Bool" $ do
            it "rendered as '+' for 'True'" $ do
                renderText (toHtml (Tagged True :: Tagged Simple Bool)) `shouldBe` "+"
            it "rendered as '-' for 'False'" $ do
                renderText (toHtml (Tagged False :: Tagged Simple Bool)) `shouldBe` "-"
        it "render Symbol as Text: Tagged '(Simple, n::Symbol) ()" $ do
            renderText (toHtml (Tagged () :: Tagged '(Simple, "test") ()))
                `shouldBe` "test"

    describe "Rendering for Input: (Simple, ids, Input ias)" $ do
        it "render Maybe Num as number" $ do
            renderText (toHtml (Tagged (Just 1)
                    :: Tagged '(Simple, '[1], Input '[]) (Maybe Int)))
                `shouldBe` "<input value=\"1\" id=\"1\" type=\"number\">"
            renderText (toHtml (Tagged Nothing
                    :: Tagged '(Simple, '[1], Input '[]) (Maybe Float)))
                `shouldBe` "<input id=\"1\" type=\"number\">"
        it "render Maybe Bool as checkbox, checked if Just True; otherwise unchecked" $ do
            renderText (toHtml (Tagged (Just True)
                    :: Tagged '(Simple, '[1], Input '[]) (Maybe Bool)))
                `shouldBe` "<input checked id=\"1\" type=\"checkbox\">"
            renderText (toHtml (Tagged (Just False)
                    :: Tagged '(Simple, '[1], Input '[]) (Maybe Bool)))
                `shouldBe` "<input id=\"1\" type=\"checkbox\">"
            renderText (toHtml (Tagged Nothing
                    :: Tagged '(Simple, '[1], Input '[]) (Maybe Bool)))
                `shouldBe` "<input id=\"1\" type=\"checkbox\">"
        it "render Maybe Text or Maybe String as text" $ do
            renderText (toHtml (Tagged (Just "test")
                    :: Tagged '(Simple, '[1], Input '[]) (Maybe String)))
                `shouldBe` "<input value=\"test\" id=\"1\">"
            renderText (toHtml (Tagged (Just "test")
                    :: Tagged '(Simple, '[1], Input '[]) (Maybe Text)))
                `shouldBe` "<input value=\"test\" id=\"1\">"
        it "render non-Maybe value as Just val" $ do
            renderText (toHtml (Tagged True
                    :: Tagged '(Simple, '[1], Input '[]) (Bool)))
                `shouldBe` "<input checked id=\"1\" type=\"checkbox\">"
        context "when added InputAttribute ReadOnly" $ do
            it "added readonly to input field" $ do
                renderText (toHtml (Tagged 1
                        :: Tagged '(Simple, '[1], Input '[ReadOnly]) Float))
                    `shouldBe` "<input value=\"1.0\" id=\"1\" type=\"number\" readonly>"
        it "added Any Attribute to input field" $ do
            renderText (toHtml (Tagged 1
                    :: Tagged '(Simple, '[1], Input '[ReadOnly, Attr "size" "120"]) Float))
                `shouldBe` "<input size=\"120\" value=\"1.0\" id=\"1\" type=\"number\" readonly>"
        it "render Hidden values" $ do
            renderText (toHtml (Tagged (Just "test")
                    :: Tagged '(Simple, '[1], Hidden) (Maybe Text)))
                `shouldBe` "<input value=\"test\" id=\"1\" type=\"hidden\">"
    describe "Rendering for Choose value: (Simple, Choose vts ias)" $ do
        it "render type Choose with list of (value, text) pair as listbox (select tag)" $ do
            renderText (toHtml (Tagged Nothing
                    :: Tagged '( Simple
                               , '[1]
                               , Choose '[ '("1","One"),'("2","Two")] '[]
                               )
                               (Maybe Int)
                    ))
                `shouldBe` mconcat
                    [ "<select id=\"1\"><option value=\"1\">One</option>"
                    , "<option value=\"2\">Two</option></select>"
                    ]
        it "set 'selected' for corresponding value" $ do
            renderText (toHtml (Tagged (Just 2) :: Tagged
                '(Simple, '[1], Choose '[ '("1","One"),'("2","Two")] '[]) (Maybe Int)))
                `shouldBe` mconcat
                    [ "<select id=\"1\"><option value=\"1\">One</option>"
                    , "<option value=\"2\" selected>Two</option></select>"
                    ]
        it "disabled if added InputAttribute ReadOnly" $ do
            renderText (toHtml (Tagged (Just 2)
                :: Tagged  '(Simple
                            , '[1]
                            , Choose '[ '("1","One"),'("2","Two")]
                                     '[ReadOnly]
                            )
                            (Maybe Int)
                    ))
                `shouldBe` mconcat
                    [ "<select disabled id=\"1\"><option value=\"1\">One</option>"
                    , "<option value=\"2\" selected>Two</option></select>"
                    ]
        it "it added Any Attribute to select" $ do
            renderText (toHtml (Tagged (Just 2)
                    :: Tagged  '(Simple
                                , '[1]
                                , Choose '[ '("1","One"),'("2","Two")]
                                         '[Attr "size" "200"]
                                )
                                (Maybe Int)
                        ))
                `shouldBe` mconcat
                    [ "<select size=\"200\" id=\"1\"><option value=\"1\">One</option>"
                    , "<option value=\"2\" selected>Two</option></select>"
                    ]
    describe "Rendering with label: (Simple, l::Symbol, t::HtmlTag)" $ do
        it "render field into label" $ do
            renderText (toHtml (Tagged (Just "test") :: Tagged '(Simple, '[1], "test", Input '[]) (Maybe Text)))
                `shouldBe` "<label>test: <input value=\"test\" id=\"1\"></label>"
            renderText (toHtml (Tagged (Just 2)
                    :: Tagged  '(Simple
                                , '[1]
                                , "test", Choose '[ '("1","One"),'("2","Two")]
                                                 '[ReadOnly]
                                )
                                (Maybe Int)
                    ))
                `shouldBe` mconcat
                    [ "<label>test: <select disabled id=\"1\"><option value=\"1\">One</option>"
                    , "<option value=\"2\" selected>Two</option></select></label>"
                    ]
        it "render Hidden field with label. In this case Label is hidden and field also has 'hidden' type" $ do
            renderText (toHtml (Tagged (Just "test") :: Tagged '(Simple, '[1], "name", Hidden) (Maybe T.Text)))
                `shouldBe` "<label hidden>name: <input value=\"test\" id=\"1\" type=\"hidden\"></label>"
            renderText (toHtml (Tagged (Nothing) :: Tagged '(Simple, '[1], "name", Hidden) (Maybe T.Text)))
                `shouldBe` "<label hidden>name: <input id=\"1\" type=\"hidden\"></label>"
            renderText (toHtml (Tagged ("test") :: Tagged '(Simple, '[1], "name", Hidden) T.Text))
                `shouldBe` "<label hidden>name: <input value=\"test\" id=\"1\" type=\"hidden\"></label>"

    describe "Rendering as table row: (Simple, (l::Symbol, t::HtmlTag))" $ do
        it "render table row" $ do
            renderText (toHtml (Tagged (Just "test") :: Tagged '(Simple, '[1], '("test", Input '[])) (Maybe Text)))
                `shouldBe` "<tr><td>test: </td><td><input value=\"test\" id=\"1\"></td></tr>"
            renderText (toHtml (Tagged (Just 2)
                :: Tagged  '( Simple
                            , '[1]
                            ,  '( "test"
                                , Choose '[ '("1","One"),'("2","Two")] '[ReadOnly]
                                )
                            )
                            (Maybe Int)
                ))
                `shouldBe` mconcat
                    [ "<tr><td>test: </td><td><select disabled id=\"1\">"
                    , "<option value=\"1\">One</option>"
                    , "<option value=\"2\" selected>Two</option>"
                    , "</select></td></tr>"
                    ]
        it "render Hidden field as row. In this case row is hidden and field also has 'hidden' type. But cells are not hidden" $ do
            renderText (toHtml (Tagged (Just "test") :: Tagged '(Simple, '[1], '("name", Hidden)) (Maybe Text)))
                `shouldBe` "<tr hidden><td>name: </td><td><input value=\"test\" id=\"1\" type=\"hidden\"></td></tr>"
            renderText (toHtml (Tagged Nothing :: Tagged '(Simple, '[1], '("name", Hidden)) (Maybe Text)))
                `shouldBe` "<tr hidden><td>name: </td><td><input id=\"1\" type=\"hidden\"></td></tr>"
            renderText (toHtml (Tagged ("test") :: Tagged '(Simple, '[1], '("name", Hidden)) Text))
                `shouldBe` "<tr hidden><td>name: </td><td><input value=\"test\" id=\"1\" type=\"hidden\"></td></tr>"
    describe "Rendering record as table: (Simple, '[ '(l::Symbol, t::HtmlTag)])" $ do
        it "render empty table (only for Maybe ())" $ do
            renderText (toHtml (Tagged (Just ()) :: TRS '[1] '[] ())) `shouldBe` ""
        it "render non-empty table  for Maybe (x,xs)" $ do
            renderText (
                toHtml (Tagged (Just ("xxx",(1,())))
                        :: TRS '[1] '[ '("one", Hidden), '("two",Input '[])]
                            (Text,(Double,()))
                        ))
                `shouldBe` mconcat
                    [ "<table><tr hidden><td>one: </td><td>"
                    , "<input value=\"xxx\" id=\"1-1\" type=\"hidden\"></td></tr><tr>"
                    , "<td>two: </td><td><input value=\"1.0\" id=\"1-2\" type=\"number\">"
                    , "</td></tr></table>"
                    ]
            renderText (
                toHtml (Tagged Nothing
                        :: TRS '[1] '[ '("one", Hidden), '("two",Input '[])]
                            (Text,(Double,()))
                        ))
                `shouldBe` mconcat
                    [ "<table><tr hidden><td>one: </td><td>"
                    , "<input id=\"1-1\" type=\"hidden\"></td></tr><tr><td>two: </td>"
                    , "<td><input id=\"1-2\" type=\"number\"></td></tr></table>"
                    ]
        it "render non-empty table  for (x,xs)" $ do
            renderText (
                toHtml (Tagged ("xxx",(1,()))
                        :: Tagged '(Simple
                                    , '[1]
                                    ,  '[ '("one", Hidden)
                                        , '("two",Input '[])
                                        ]) (Text,(Double,()))
                        ))
                `shouldBe` mconcat
                    [ "<table><tr hidden><td>one: </td><td>"
                    , "<input value=\"xxx\" id=\"1-1\" type=\"hidden\"></td></tr><tr>"
                    , "<td>two: </td><td><input value=\"1.0\" id=\"1-2\" type=\"number\">"
                    , "</td></tr></table>"
                    ]
            renderText (
                toHtml (Tagged ("xxx",(1,()))
                        :: Tagged '(Simple
                                    , '[1]
                                    ,  '[ '("one", Choose '[ '("1","One"),'("2","Two")] '[])
                                        , '("two",Input '[])
                                        ]
                                    ) (Text,(Double,()))
                        ))
                `shouldBe` mconcat
                    [ "<table><tr><td>one: </td><td><select id=\"1-1\">"
                    , "<option value=\"1\">One</option>"
                    , "<option value=\"2\">Two</option></select></td></tr>"
                    , "<tr><td>two: </td><td><input value=\"1.0\" id=\"1-2\" type=\"number\">"
                    , "</td></tr></table>"
                    ]
    describe "Rendering list for input as table: (Simple, '[ '(l::Symbol, t::HtmlTag)]) [x]" $ do
        it "render list" $ do
            renderText (
                toHtml (Tagged [("v1",("2",(1,()))),("v2",("1",(2,())))]
                        :: Tagged  '(Simple
                                    , '[1]
                                    , '[ '("i1", Input '[])
                                        , '("c1", Choose '[ '("1","One"),'("2","Two")] '[])
                                        , '("i2", Input '[ReadOnly])
                                        ]
                                    ) [(Text,(Text,(Int,())))]
                        ))
                `shouldBe` mconcat
                        [ "<table><tr><th>i1</th><th>c1</th><th>i2</th></tr>"
                        , "<tr><td><input value=\"v1\" id=\"1-1-1\"></td><td>"
                        , "<select id=\"1-1-2\"><option value=\"1\">One</option>"
                        , "<option value=\"2\" selected>Two</option>"
                        , "</select></td><td>"
                        , "<input value=\"1\" id=\"1-1-3\" type=\"number\" readonly></td></tr>"
                        , "<tr><td><input value=\"v2\" id=\"1-2-1\"></td><td>"
                        , "<select id=\"1-2-2\"><option value=\"1\" selected>One</option>"
                        , "<option value=\"2\">Two</option>"
                        , "</select></td><td>"
                        , "<input value=\"2\" id=\"1-2-3\" type=\"number\" readonly>"
                        , "</td></tr></table>"
                        ]
    describe "Rendering list for output as table: (Simple, '[Symbol]) [x]" $ do
        it "render list" $ do
            renderText (
                toHtml (Tagged [("v1",("2",(1,()))),("v2",("1",(2,())))]
                        :: Tagged  '(Simple, '["i1", "c1","i2"])
                                    [(Text,(Text,(Int,())))]
                        ))
                `shouldBe` mconcat
                    [ "<table><tr><th>i1</th><th>c1</th><th>i2</th></tr>"
                    , "<tr><td>v1</td><td>2</td><td>1</td></tr>"
                    , "<tr><td>v2</td><td>1</td><td>2</td></tr></table>"
                    ]

