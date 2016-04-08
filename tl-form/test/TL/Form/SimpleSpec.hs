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

-- type TRS  (rs :: [(Symbol, HtmlTag)]) v = Tagged '(Simple, rs) (Maybe v)

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
            renderText1 (toTLF (Tagged (Just 1) :: IInput '[] (Maybe Int)))
                `shouldBe` "<input value=\"1\" type=\"number\">"
            renderText1 (toTLF (Tagged Nothing :: IInput '[] (Maybe Float)))
                `shouldBe` "<input type=\"number\">"
        
        it "render Maybe Bool as checkbox, checked if Just True; otherwise unchecked" $ do
            renderText1 (toTLF (Tagged (Just True) :: IInput '[] (Maybe Bool)))
                `shouldBe` "<input checked type=\"checkbox\">"
            renderText1 (toTLF (Tagged (Just False) :: IInput '[] (Maybe Bool)))
                `shouldBe` "<input type=\"checkbox\">"
            renderText1 (toTLF (Tagged Nothing :: IInput '[] (Maybe Bool)))
                `shouldBe` "<input type=\"checkbox\">"
        
        it "render Maybe Text or Maybe String as text" $ do
            renderText1 (toTLF (Tagged (Just "test") :: IInput '[] (Maybe String)))
                `shouldBe` "<input value=\"test\">"
            renderText1 (toTLF (Tagged (Just "test") :: IInput '[] (Maybe Text)))
                `shouldBe` "<input value=\"test\">"
        
        it "render non-Maybe value as Just val" $ do
            renderText1 (toTLF (Tagged True :: IInput '[] Bool))
                `shouldBe` "<input checked type=\"checkbox\">"
        
        context "when added InputAttribute ReadOnly" $ do
            it "added readonly to input field" $ do
                renderText1 (toTLF (Tagged (Just 1) :: IInput '[ReadOnly] (Maybe Float)))
                    `shouldBe` "<input value=\"1.0\" type=\"number\" readonly>"
        
        it "added Any Attribute to input field" $ do
            renderText1 (toTLF (Tagged (Just 1)
                :: IInput '[ReadOnly, Attr "size" "120"] (Maybe Float)))
                `shouldBe` "<input size=\"120\" value=\"1.0\" type=\"number\" readonly>"
                
        it "render Hidden values" $ do
            renderText1 (toTLF (Tagged (Just "test") :: IHidden (Maybe Text)))
                `shouldBe` "<input value=\"test\" type=\"hidden\">"
                
    describe "Rendering for Choose value: (Simple, Choose vts ias)" $ do
        it "render type Choose with list of (value, text) pair as listbox (select tag)" $ do
            renderText1 (toTLF (Tagged Nothing
                    :: IChoose '[ '("1","One"),'("2","Two")] '[] (Maybe Int)))
                `shouldBe` mconcat
                    [ "<select><option value=\"1\">One</option>"
                    , "<option value=\"2\">Two</option></select>"
                    ]
        
        it "set 'selected' for corresponding value" $ do
            renderText1 (toTLF (Tagged (Just 2) 
                    :: IChoose '[ '("1","One"),'("2","Two")] '[] (Maybe Int)))
                `shouldBe` mconcat
                    [ "<select><option value=\"1\">One</option>"
                    , "<option value=\"2\" selected>Two</option></select>"
                    ]
        
        it "disabled if added InputAttribute ReadOnly" $ do
            renderText1 (toTLF (Tagged (Just 2) :: IChs '[ReadOnly] (Maybe Int)))
                `shouldBe` mconcat
                    [ "<select disabled><option value=\"1\">One</option>"
                    , "<option value=\"2\" selected>Two</option></select>"
                    ]
        
        it "it added Any Attribute to select" $ do
            renderText1 (toTLF (Tagged (Just 2) :: IChs '[Attr "size" "200"] (Maybe Int)))
                `shouldBe` mconcat
                    [ "<select size=\"200\"><option value=\"1\">One</option>"
                    , "<option value=\"2\" selected>Two</option></select>"
                    ]
                    
    describe "Rendering with label: (Simple, l::Symbol, t::HtmlTag)" $ do
        it "render field into label" $ do
            renderText1 (toTLF (Tagged (Just "val") 
                    :: TLblTag "test" (Input '[]) (Maybe Text)))
                `shouldBe` "<label name=\"test\">test: <input value=\"val\"></label>"
            renderText1 (toTLF (Tagged (Just 2)
                    :: TLblTag "test" (Chs '[ReadOnly]) (Maybe Int)))
                `shouldBe` mconcat
                    [ "<label name=\"test\">test: <select disabled>"
                    , "<option value=\"1\">One</option>"
                    , "<option value=\"2\" selected>Two</option></select></label>"
                    ]
        
        it "render Hidden field with label. In this case Label is hidden and field also has 'hidden' type" $ do
            renderText1 (toTLF (Tagged (Just "test") 
                    :: TLblTag "name" Hidden (Maybe T.Text)))
                `shouldBe` mconcat
                    [ "<label name=\"name\" hidden>name: "
                    , "<input value=\"test\" type=\"hidden\"></label>"
                    ]
            renderText1 (toTLF (Tagged (Nothing) 
                    :: TLblTag "name" Hidden (Maybe T.Text)))
                `shouldBe` mconcat
                    [ "<label name=\"name\" hidden>"
                    , "name: <input type=\"hidden\"></label>"
                    ]
            renderText1 (toTLF (Tagged ("test") 
                    :: TLblTag "name" Hidden T.Text))
                `shouldBe` mconcat
                    [ "<label name=\"name\" hidden>name: "
                    , "<input value=\"test\" type=\"hidden\"></label>"
                    ]
    describe "Rendering as table row: (Simple, (l::Symbol, t::HtmlTag))" $ do
        it "render table row" $ do
            renderText1 (toTLF (Tagged (Just "val") 
                    :: TRowLblTag "test" (Input '[]) (Maybe Text)))
                `shouldBe` mconcat
                    [ "<tr name=\"test\"><td>test: </td><td>"
                    , "<input value=\"val\"></td></tr>"
                    ]
            renderText1 (toTLF (Tagged (Just 2)
                    :: TRowLblTag "test" (Chs '[ReadOnly]) (Maybe Int)))
                `shouldBe` mconcat
                    [ "<tr name=\"test\"><td>test: </td><td><select disabled>"
                    , "<option value=\"1\">One</option>"
                    , "<option value=\"2\" selected>Two</option>"
                    , "</select></td></tr>"
                    ]
        
        it "render Hidden field as row. In this case row is hidden and field also has 'hidden' type. But cells are not hidden" $ do
            renderText1 (toTLF (Tagged (Just "test") 
                    :: TRowLblTag "name" Hidden (Maybe Text)))
                `shouldBe` mconcat
                    [ "<tr name=\"name\" hidden><td>name: </td>"
                    , "<td><input value=\"test\" type=\"hidden\"></td></tr>"
                    ]
            renderText1 (toTLF (Tagged Nothing 
                    :: TRowLblTag "name" Hidden (Maybe Text)))
                `shouldBe` mconcat
                    [ "<tr name=\"name\" hidden><td>name: </td><td>"
                    , "<input type=\"hidden\"></td></tr>"
                    ]
            renderText1 (toTLF (Tagged ("test") 
                    :: TRowLblTag "name" Hidden Text))
                `shouldBe` mconcat
                    [ "<tr name=\"name\" hidden><td>name: </td><td>"
                    , "<input value=\"test\" type=\"hidden\"></td></tr>"
                    ]
    
    describe "Rendering record as table: (Simple, '[ '(l::Symbol, t::HtmlTag)])" $ do
    
        it "render empty table (only for Maybe ())" $ do
            renderText1 (toTLF (Tagged (Just ()) :: TRecAsTab '[] (Maybe ()))) `shouldBe` ""
    
        it "render non-empty table  for Maybe (x,xs)" $ do
            renderText1 (
                toTLF (Tagged (Just ("xxx",(1,())))
                        :: TRecAsTab '[ '("one", Hidden), '("two",Input '[])]
                                    (Maybe (Text,(Double,())))
                        ))
                `shouldBe` mconcat
                    [ "<table><tr name=\"one\" hidden><td>one: </td><td>"
                    , "<input value=\"xxx\" type=\"hidden\"></td></tr>"
                    , "<tr name=\"two\"><td>two: </td><td>"
                    , "<input value=\"1.0\" type=\"number\"></td></tr></table>"
                    ]
            renderText1 (
                toTLF (Tagged Nothing
                        :: TRecAsTab '[ '("one", Hidden), '("two",Input '[])]
                                    (Maybe (Text,(Double,())))
                        ))
                `shouldBe` mconcat
                    [ "<table><tr name=\"one\" hidden><td>one: </td><td>"
                    , "<input type=\"hidden\"></td></tr>"
                    , "<tr name=\"two\"><td>two: </td><td>"
                    , "<input type=\"number\"></td></tr></table>"
                    ]
    
        it "render non-empty table  for (x,xs)" $ do
            renderText1 (
                toTLF (Tagged ("xxx",(1,()))
                        :: TRecAsTab  '[ '("one", Hidden)
                                       , '("two",Input '[])
                                       ] 
                                       (Text,(Double,()))
                        ))
                `shouldBe` mconcat
                    [ "<table><tr name=\"one\" hidden><td>one: </td><td>"
                    , "<input value=\"xxx\" type=\"hidden\"></td></tr>"
                    , "<tr name=\"two\"><td>two: </td><td>"
                    , "<input value=\"1.0\" type=\"number\"></td></tr></table>"
                    ]
            renderText1 (
                toTLF (Tagged ("xxx",(1,()))
                        :: TRecAsTab '[ '("one", Chs '[])
                                      , '("two",Input '[])
                                      ] 
                                      (Text,(Double,()))
                        ))
                `shouldBe` mconcat
                    [ "<table><tr name=\"one\"><td>one: </td><td><select>"
                    , "<option value=\"1\">One</option>"
                    , "<option value=\"2\">Two</option></select></td></tr>"
                    , "<tr name=\"two\"><td>two: </td><td>"
                    , "<input value=\"1.0\" type=\"number\"></td></tr></table>"
                    ]
    
    describe "Rendering list for input as table: (Simple, '[ '(l::Symbol, t::HtmlTag)]) [x]" $ do
        it "render list" $ do
            renderText1 (
                toTLF (Tagged [("v1",("2",(1,()))),("v2",("1",(2,())))]
                        :: TTable  '[ '("i1", Input '[])
                                    , '("c1", Chs '[])
                                    , '("i2", Input '[ReadOnly])
                                    ]
                                    [(Text,(Text,(Int,())))]
                        ))
                `shouldBe` mconcat
                        [ "<table><tr><th name=\"i1\">i1</th><th name=\"c1\">c1"
                        , "</th><th name=\"i2\">i2</th></tr>"
                        , "<tr><td name=\"i1\"><input value=\"v1\"></td><td name=\"c1\">"
                        , "<select><option value=\"1\">One</option>"
                        , "<option value=\"2\" selected>Two</option>"
                        , "</select></td><td name=\"i2\">"
                        , "<input value=\"1\" type=\"number\" readonly></td></tr>"
                        , "<tr><td name=\"i1\"><input value=\"v2\"></td><td name=\"c1\">"
                        , "<select><option value=\"1\" selected>One</option>"
                        , "<option value=\"2\">Two</option>"
                        , "</select></td><td name=\"i2\">"
                        , "<input value=\"2\" type=\"number\" readonly>"
                        , "</td></tr></table>"
                        ]
    describe "Rendering list for output as table: (Simple, '[Symbol]) [x]" $ do
        it "render list" $ do
            renderText1 (
                toTLF (Tagged [("v1",("2",(1,()))),("v2",("1",(2,())))]
                        :: TTableReadOnly '["i1", "c1","i2"] [(Text,(Text,(Int,())))]
                        ))
                `shouldBe` mconcat
                    [ "<table><tr><th name=\"i1\">i1</th><th name=\"c1\">c1"
                    , "</th><th name=\"i2\">i2</th></tr>"
                    , "<tr><td name=\"i1\">v1</td><td name=\"c1\">2</td>"
                    , "<td name=\"i2\">1</td></tr><tr><td name=\"i1\">v2</td>"
                    , "<td name=\"c1\">1</td><td name=\"i2\">2</td></tr></table>"
                    ]
    describe "Records and tables can be composed" $ do
        it "render list of tables one after another" $ do
            renderText1 (
                    toTLF (Tagged $
                            ( ([("v1",("2",(1,()))),("v2",("1",(2,())))] ,)
                            . ([("v1",("2",(1,(False,())))),("v2",("1",(2,(True,()))))] ,)
                            . (("xxx",(1,())) ,)
                            ) $ ()
                            :: TGroup 
                                '[ Table '[ '("i1", None)
                                          , '("c1", None)
                                          , '("i2", Input '[ReadOnly])
                                          ]
                                , Table  '[ '("i1", Input '[])
                                          , '("c1", Chs '[])
                                          , '("i2", Input '[ReadOnly])
                                          , '("check", Input '[])
                                          ]
                                , RecAsTab  '[ '("one", Chs '[])
                                             , '("two", Input '[])
                                             ]
                                ]
                                ( [ (Text,(Text,(Int,())))]
                                  , ( [(Text,(Text,(Int,(Bool,()))))]
                                    , ( (Text,(Double,()))
                                      , ()
                                      )
                                    )
                                )
                            ))
                    `shouldBe` mconcat
                        [ "<table>"
                        , "<tr><th name=\"i1\">i1</th><th name=\"c1\">c1"
                        , "</th><th name=\"i2\">i2</th></tr>"
                        , "<tr><td name=\"i1\">v1</td><td name=\"c1\">2</td>"
                        , "<td name=\"i2\"><input value=\"1\" "
                        , "type=\"number\" readonly></td></tr>"
                        , "<tr><td name=\"i1\">v2</td><td name=\"c1\">1</td>"
                        , "<td name=\"i2\"><input value=\"2\""
                        , " type=\"number\" readonly></td></tr></table>"
                        
                        , "<table>"
                        , "<tr><th name=\"i1\">i1</th><th name=\"c1\">c1"
                        , "</th><th name=\"i2\">i2</th><th name=\"check\">check"
                        , "</th></tr>"
                        , "<tr><td name=\"i1\"><input value=\"v1\">"
                        , "</td><td name=\"c1\"><select><option value=\"1\">One"
                        , "</option><option value=\"2\" selected>Two</option>"
                        , "</select></td><td name=\"i2\"><input value=\"1\" "
                        , "type=\"number\" readonly></td><td name=\"check\">"
                        , "<input type=\"checkbox\"></td></tr>"
                        , "<tr><td name=\"i1\"><input value=\"v2\"></td>"
                        , "<td name=\"c1\"><select>"
                        , "<option value=\"1\" selected>One</option>"
                        , "<option value=\"2\">Two</option></select></td>"
                        , "<td name=\"i2\">"
                        , "<input value=\"2\" type=\"number\" readonly>"
                        , "</td><td name=\"check\">"
                        , "<input checked type=\"checkbox\"></td></tr></table>"
                        , "<table><tr name=\"one\"><td>one: </td><td><select>"
                        , "<option value=\"1\">One</option><option value=\"2\">"
                        , "Two</option></select></td></tr><tr name=\"two\">"
                        , "<td>two: </td><td><input value=\"1.0\" "
                        , "type=\"number\"></td></tr></table>"
                        ]
        it "render table as element" $ do
            renderText1 (
                    toTLF (Tagged
                            [("v1",(("rec",(False,())),(1,())))
                            ,("v2",(("???",(True, ())),(2,())))
                            ]
                        :: TTable '[ '("i1", Input '[])
                                   , '("c1", Rec '[ '("name", Input '[])
                                                  , '("c1"  , Input '[])
                                                  ])
                                   , '("i2", Input '[ReadOnly])
                                   ]
                                [(Text,((Text,(Bool,())),(Int,())))]
                        ))
                    `shouldBe` mconcat 
                        [ "<table>"
                        , "<tr><th name=\"i1\">i1</th><th name=\"c1\">c1"
                        , "</th><th name=\"i2\">i2</th></tr>"
                        , "<tr><td name=\"i1\"><input value=\"v1\"></td>"
                        , "<td name=\"c1\">"
                        , "<table><tr name=\"c1-name\"><td>name: </td><td>"
                        , "<input value=\"rec\"></td></tr><tr name=\"c1-c1\">"
                        , "<td>c1: </td><td><input "
                        , "type=\"checkbox\"></td></tr></table></td>"
                        , "<td name=\"i2\"><input value=\"1\" type=\"number\" "
                        , "readonly></td></tr>"
                        , "<tr><td name=\"i1\"><input value=\"v2\">"
                        , "</td><td name=\"c1\"><table><tr name=\"c1-name\">"
                        , "<td>name: </td><td><input value=\"???\"></td></tr>"
                        , "<tr name=\"c1-c1\"><td>c1: </td><td><input checked "
                        , "type=\"checkbox\"></td></tr></table></td>"
                        , "<td name=\"i2\"><input value=\"2\" type=\"number\" "
                        , "readonly></td></tr></table>"
                        ]
                               

type Chs = Choose '[ '("1","One"),'("2","Two")]
type IChs a b = IChoose '[ '("1","One"),'("2","Two")] a b
