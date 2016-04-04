{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
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

type TRS  (rs :: [(Symbol, HtmlTag)]) v = Tagged '(Simple, rs) (Maybe v)

spec :: Spec
spec = describe "Simple rendering" $ do
    describe "For output-only values" $ do
        it "render Int" $ do
            renderText1 (toTLF (Tagged 1 :: Tagged Simple Int))
                `shouldBe` "1"
        it "render Integer" $ do
            renderText1 (toTLF (Tagged 12345678901234567890 :: Tagged Simple Integer))
                `shouldBe` "12345678901234567890"
        it "render Float" $ do
            renderText1 (toTLF (Tagged 1 :: Tagged Simple Float))
                `shouldBe` "1.0"
        it "render Double" $ do
            renderText1 (toTLF (Tagged 1 :: Tagged Simple Double))
                `shouldBe` "1.0"
        it "render String" $ do
            renderText1 (toTLF (Tagged "русский язык" :: Tagged Simple String))
                `shouldBe` "русский язык"
        it "render Text" $ do
            renderText1 (toTLF (Tagged "עברית" :: Tagged Simple Text))
                `shouldBe` "עברית"
        context "with Maybe a" $ do
            it "render having value" $ do
                renderText1 (toTLF (Tagged (Just 1) :: Tagged Simple (Maybe Int)))
                    `shouldBe` "1"
            it "render having no value" $ do
                renderText1 (toTLF (Tagged Nothing :: Tagged Simple (Maybe Text)))
                    `shouldBe` ""
        context "when rendering Bool" $ do
            it "rendered as '+' for 'True'" $ do
                renderText1 (toTLF (Tagged True :: Tagged Simple Bool)) `shouldBe` "+"
            it "rendered as '-' for 'False'" $ do
                renderText1 (toTLF (Tagged False :: Tagged Simple Bool)) `shouldBe` "-"
        it "render Symbol as Text: Tagged '(Simple, n::Symbol) ()" $ do
            renderText1 (toTLF (Tagged () :: Tagged '(Simple, "test") ()))
                `shouldBe` "test"

    describe "Rendering for Input: (Simple, ids, Input ias)" $ do
        it "render Maybe Num as number" $ do
            renderText1 (toTLF (Tagged (Just 1)
                    :: Tagged '(Simple, Input '[]) (Maybe Int)))
                `shouldBe` "<input value=\"1\" id=\"1\" type=\"number\">"
            renderText1 (toTLF (Tagged Nothing
                    :: Tagged '(Simple, Input '[]) (Maybe Float)))
                `shouldBe` "<input id=\"1\" type=\"number\">"
        it "render Maybe Bool as checkbox, checked if Just True; otherwise unchecked" $ do
            renderText1 (toTLF (Tagged (Just True)
                    :: Tagged '(Simple, Input '[]) (Maybe Bool)))
                `shouldBe` "<input checked id=\"1\" type=\"checkbox\">"
            renderText1 (toTLF (Tagged (Just False)
                    :: Tagged '(Simple, Input '[]) (Maybe Bool)))
                `shouldBe` "<input id=\"1\" type=\"checkbox\">"
            renderText1 (toTLF (Tagged Nothing
                    :: Tagged '(Simple, Input '[]) (Maybe Bool)))
                `shouldBe` "<input id=\"1\" type=\"checkbox\">"
        it "render Maybe Text or Maybe String as text" $ do
            renderText1 (toTLF (Tagged (Just "test")
                    :: Tagged '(Simple, Input '[]) (Maybe String)))
                `shouldBe` "<input value=\"test\" id=\"1\">"
            renderText1 (toTLF (Tagged (Just "test")
                    :: Tagged '(Simple, Input '[]) (Maybe Text)))
                `shouldBe` "<input value=\"test\" id=\"1\">"
        it "render non-Maybe value as Just val" $ do
            renderText1 (toTLF (Tagged True
                    :: Tagged '(Simple, Input '[]) (Bool)))
                `shouldBe` "<input checked id=\"1\" type=\"checkbox\">"
        context "when added InputAttribute ReadOnly" $ do
            it "added readonly to input field" $ do
                renderText1 (toTLF (Tagged 1
                        :: Tagged '(Simple, Input '[ReadOnly]) Float))
                    `shouldBe` "<input value=\"1.0\" id=\"1\" type=\"number\" readonly>"
        it "added Any Attribute to input field" $ do
            renderText1 (toTLF (Tagged 1
                    :: Tagged '(Simple, Input '[ReadOnly, Attr "size" "120"]) Float))
                `shouldBe` "<input size=\"120\" value=\"1.0\" id=\"1\" type=\"number\" readonly>"
        it "render Hidden values" $ do
            renderText1 (toTLF (Tagged (Just "test")
                    :: Tagged '(Simple, Hidden) (Maybe Text)))
                `shouldBe` "<input value=\"test\" id=\"1\" type=\"hidden\">"
    describe "Rendering for Choose value: (Simple, Choose vts ias)" $ do
        it "render type Choose with list of (value, text) pair as listbox (select tag)" $ do
            renderText1 (toTLF (Tagged Nothing
                    :: Tagged '( Simple
                               , Choose '[ '("1","One"),'("2","Two")] '[]
                               )
                               (Maybe Int)
                    ))
                `shouldBe` mconcat
                    [ "<select id=\"1\"><option value=\"1\">One</option>"
                    , "<option value=\"2\">Two</option></select>"
                    ]
        it "set 'selected' for corresponding value" $ do
            renderText1 (toTLF (Tagged (Just 2) :: Tagged
                '(Simple, Choose '[ '("1","One"),'("2","Two")] '[]) (Maybe Int)))
                `shouldBe` mconcat
                    [ "<select id=\"1\"><option value=\"1\">One</option>"
                    , "<option value=\"2\" selected>Two</option></select>"
                    ]
        it "disabled if added InputAttribute ReadOnly" $ do
            renderText1 (toTLF (Tagged (Just 2)
                :: Tagged  '(Simple
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
            renderText1 (toTLF (Tagged (Just 2)
                    :: Tagged  '(Simple
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
            renderText1 (toTLF (Tagged (Just "test") :: Tagged '(Simple, "test", Input '[]) (Maybe Text)))
                `shouldBe` "<label>test: <input value=\"test\" id=\"1\"></label>"
            renderText1 (toTLF (Tagged (Just 2)
                    :: Tagged  '(Simple
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
            renderText1 (toTLF (Tagged (Just "test") :: Tagged '(Simple, "name", Hidden) (Maybe T.Text)))
                `shouldBe` "<label hidden>name: <input value=\"test\" id=\"1\" type=\"hidden\"></label>"
            renderText1 (toTLF (Tagged (Nothing) :: Tagged '(Simple, "name", Hidden) (Maybe T.Text)))
                `shouldBe` "<label hidden>name: <input id=\"1\" type=\"hidden\"></label>"
            renderText1 (toTLF (Tagged ("test") :: Tagged '(Simple, "name", Hidden) T.Text))
                `shouldBe` "<label hidden>name: <input value=\"test\" id=\"1\" type=\"hidden\"></label>"

    describe "Rendering as table row: (Simple, (l::Symbol, t::HtmlTag))" $ do
        it "render table row" $ do
            renderText1 (toTLF (Tagged (Just "test") :: Tagged '(Simple, '("test", Input '[])) (Maybe Text)))
                `shouldBe` "<tr><td>test: </td><td><input value=\"test\" id=\"1\"></td></tr>"
            renderText1 (toTLF (Tagged (Just 2)
                :: Tagged  '( Simple
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
            renderText1 (toTLF (Tagged (Just "test") :: Tagged '(Simple, '("name", Hidden)) (Maybe Text)))
                `shouldBe` "<tr hidden><td>name: </td><td><input value=\"test\" id=\"1\" type=\"hidden\"></td></tr>"
            renderText1 (toTLF (Tagged Nothing :: Tagged '(Simple, '("name", Hidden)) (Maybe Text)))
                `shouldBe` "<tr hidden><td>name: </td><td><input id=\"1\" type=\"hidden\"></td></tr>"
            renderText1 (toTLF (Tagged ("test") :: Tagged '(Simple, '("name", Hidden)) Text))
                `shouldBe` "<tr hidden><td>name: </td><td><input value=\"test\" id=\"1\" type=\"hidden\"></td></tr>"
    describe "Rendering record as table: (Simple, '[ '(l::Symbol, t::HtmlTag)])" $ do
        it "render empty table (only for Maybe ())" $ do
            renderText1 (toTLF (Tagged (Just ()) :: TRS '[] ())) `shouldBe` ""
        it "render non-empty table  for Maybe (x,xs)" $ do
            renderText1 (
                toTLF (Tagged (Just ("xxx",(1,())))
                        :: TRS '[ '("one", Hidden), '("two",Input '[])]
                            (Text,(Double,()))
                        ))
                `shouldBe` mconcat
                    [ "<table><tr hidden><td>one: </td><td>"
                    , "<input value=\"xxx\" id=\"1\" type=\"hidden\"></td></tr><tr>"
                    , "<td>two: </td><td><input value=\"1.0\" id=\"2\" type=\"number\">"
                    , "</td></tr></table>"
                    ]
            renderText1 (
                toTLF (Tagged Nothing
                        :: TRS '[ '("one", Hidden), '("two",Input '[])]
                            (Text,(Double,()))
                        ))
                `shouldBe` mconcat
                    [ "<table><tr hidden><td>one: </td><td>"
                    , "<input id=\"1\" type=\"hidden\"></td></tr><tr><td>two: </td>"
                    , "<td><input id=\"2\" type=\"number\"></td></tr></table>"
                    ]
        it "render non-empty table  for (x,xs)" $ do
            renderText1 (
                toTLF (Tagged ("xxx",(1,()))
                        :: Tagged '(Simple
                                    ,  '[ '("one", Hidden)
                                        , '("two",Input '[])
                                        ]) (Text,(Double,()))
                        ))
                `shouldBe` mconcat
                    [ "<table><tr hidden><td>one: </td><td>"
                    , "<input value=\"xxx\" id=\"1\" type=\"hidden\"></td></tr><tr>"
                    , "<td>two: </td><td><input value=\"1.0\" id=\"2\" type=\"number\">"
                    , "</td></tr></table>"
                    ]
            renderText1 (
                toTLF (Tagged ("xxx",(1,()))
                        :: Tagged '(Simple
                                    ,  '[ '("one", Chs '[])
                                        , '("two",Input '[])
                                        ]
                                    ) (Text,(Double,()))
                        ))
                `shouldBe` mconcat
                    [ "<table><tr><td>one: </td><td><select id=\"1\">"
                    , "<option value=\"1\">One</option>"
                    , "<option value=\"2\">Two</option></select></td></tr>"
                    , "<tr><td>two: </td><td><input value=\"1.0\" id=\"2\" type=\"number\">"
                    , "</td></tr></table>"
                    ]
    describe "Rendering list for input as table: (Simple, '[ '(l::Symbol, t::HtmlTag)]) [x]" $ do
        it "render list" $ do
            renderText1 (
                toTLF (Tagged [("v1",("2",(1,()))),("v2",("1",(2,())))]
                        :: Tagged  '(Simple
                                    , '[  '("i1", Input '[])
                                        , '("c1", Chs '[])
                                        , '("i2", Input '[ReadOnly])
                                        ]
                                    ) [(Text,(Text,(Int,())))]
                        ))
                `shouldBe` mconcat
                        [ "<table><tr><th>i1</th><th>c1</th><th>i2</th></tr>"
                        , "<tr><td><input value=\"v1\" id=\"1\"></td><td>"
                        , "<select id=\"2\"><option value=\"1\">One</option>"
                        , "<option value=\"2\" selected>Two</option>"
                        , "</select></td><td>"
                        , "<input value=\"1\" id=\"3\" type=\"number\" readonly></td></tr>"
                        , "<tr><td><input value=\"v2\" id=\"4\"></td><td>"
                        , "<select id=\"5\"><option value=\"1\" selected>One</option>"
                        , "<option value=\"2\">Two</option>"
                        , "</select></td><td>"
                        , "<input value=\"2\" id=\"6\" type=\"number\" readonly>"
                        , "</td></tr></table>"
                        ]
    describe "Rendering list for output as table: (Simple, '[Symbol]) [x]" $ do
        it "render list" $ do
            renderText1 (
                toTLF (Tagged [("v1",("2",(1,()))),("v2",("1",(2,())))]
                        :: Tagged  '(Simple, '["i1", "c1","i2"])
                                    [(Text,(Text,(Int,())))]
                        ))
                `shouldBe` mconcat
                    [ "<table><tr><th>i1</th><th>c1</th><th>i2</th></tr>"
                    , "<tr><td>v1</td><td>2</td><td>1</td></tr>"
                    , "<tr><td>v2</td><td>1</td><td>2</td></tr></table>"
                    ]
    describe "Records and tables can be composed" $ do
        it "rendered as well" $ do
            renderText1 (
                    toTLF (Tagged $
                            ( ([("v1",("2",(1,()))),("v2",("1",(2,())))] ,)
                            . ([("v1",("2",(1,()))),("v2",("1",(2,())))] ,)
                            . (("xxx",(1,())) ,)
                            ) $ ()
                            :: Tagged  
                                '(Simple
                                , '[ '[ '("i1", None)
                                      , '("c1", None)
                                      , '("i2", Input '[ReadOnly])
                                      ]
                                   , '[ '("i1", Input '[])
                                      , '("c1", Chs '[])
                                      , '("i2", Input '[ReadOnly])
                                      ]
                                   , '[ '("one", Chs '[])
                                      , '("two", Input '[])
                                      ]
                                   ]
                                 )
                                 ([(Text,(Text,(Int,())))]
                                 ,([(Text,(Text,(Int,())))]
                                 ,((Text,(Double,()))
                                 ,())))
                            ))
                    `shouldBe` mconcat
                        [ "<table><tr><th>i1</th><th>c1</th><th>i2</th></tr>"
                        , "<tr><td>v1</td><td>2</td><td><input value=\"1\" "
                        , "id=\"1\" type=\"number\" readonly></td></tr><tr>"
                        , "<td>v2</td><td>1</td><td><input value=\"2\" id=\"2\""
                        , " type=\"number\" readonly></td></tr></table><table>"
                        , "<tr><th>i1</th><th>c1</th><th>i2</th></tr><tr><td>"
                        , "<input value=\"v1\" id=\"3\"></td><td><select "
                        , "id=\"4\"><option value=\"1\">One</option><option "
                        , "value=\"2\" selected>Two</option></select></td><td>"
                        , "<input value=\"1\" id=\"5\" type=\"number\" "
                        , "readonly></td></tr><tr><td><input value=\"v2\" "
                        , "id=\"6\"></td><td><select id=\"7\"><option "
                        , "value=\"1\" selected>One</option><option value=\"2\">"
                        , "Two</option></select></td><td><input value=\"2\" "
                        , "id=\"8\" type=\"number\" readonly></td></tr></table>"
                        , "<table><tr><td>one: </td><td><select id=\"9\">"
                        , "<option value=\"1\">One</option><option value=\"2\">"
                        , "Two</option></select></td></tr><tr><td>two: </td><td>"
                        , "<input value=\"1.0\" id=\"10\" type=\"number\">"
                        , "</td></tr></table>"
                        ]

type Chs = Choose '[ '("1","One"),'("2","Two")]
