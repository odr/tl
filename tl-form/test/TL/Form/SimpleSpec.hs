{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE PolyKinds #-}
module TL.Form.SimpleSpec where

import Test.Hspec
import TL.Form.Simple
import TL.Form.Types
import Data.Tagged
import Lucid
import Data.Text(Text)

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

    describe "Rendering for Input: (Simple, Input ias)" $ do
        it "render Maybe Num as number" $ do
            renderText (toHtml (Tagged (Just 1) :: Tagged '(Simple, Input '[]) (Maybe Int)))
                `shouldBe` "<input value=\"1\" type=\"number\">"
            renderText (toHtml (Tagged Nothing :: Tagged '(Simple, Input '[]) (Maybe Float)))
                `shouldBe` "<input type=\"number\">"
        it "render Maybe Bool as checkbox, checked if Just True; otherwise unchecked" $ do
            renderText (toHtml (Tagged (Just True) :: Tagged '(Simple, Input '[]) (Maybe Bool)))
                `shouldBe` "<input checked type=\"checkbox\">"
            renderText (toHtml (Tagged (Just False) :: Tagged '(Simple, Input '[]) (Maybe Bool)))
                `shouldBe` "<input type=\"checkbox\">"
            renderText (toHtml (Tagged Nothing :: Tagged '(Simple, Input '[]) (Maybe Bool)))
                `shouldBe` "<input type=\"checkbox\">"
        it "render Maybe Text or Maybe String as text" $ do
            renderText (toHtml (Tagged (Just "test") :: Tagged '(Simple, Input '[]) (Maybe String)))
                `shouldBe` "<input value=\"test\">"
            renderText (toHtml (Tagged (Just "test") :: Tagged '(Simple, Input '[]) (Maybe Text)))
                `shouldBe` "<input value=\"test\">"
        it "render non-Maybe value as Just val" $ do
            renderText (toHtml (Tagged True :: Tagged '(Simple, Input '[]) (Bool)))
                `shouldBe` "<input checked type=\"checkbox\">"
        context "when added InputAttribute ReadOnly" $ do
            it "added readonly to input field" $ do
                renderText (toHtml (Tagged 1 :: Tagged '(Simple, Input '[ReadOnly]) Float))
                    `shouldBe` "<input value=\"1.0\" type=\"number\" readonly>"
    describe "Rendering for Choose value: (Simple, Choose vts ias)" $ do
        it "render type Choose with list of (value, text) pair as listbox (select tag)" $ do
            renderText (toHtml (Tagged Nothing :: Tagged
                '(Simple, Choose '[ '("1","One"),'("2","Two")] '[]) (Maybe Int)))
                `shouldBe` "<select><option value=\"1\">One</option><option value=\"2\">Two</option></select>"
        it "set 'selected' for corresponding value" $ do
            renderText (toHtml (Tagged (Just 2) :: Tagged
                '(Simple, Choose '[ '("1","One"),'("2","Two")] '[]) (Maybe Int)))
                `shouldBe` "<select><option value=\"1\">One</option><option value=\"2\" selected>Two</option></select>"
        it "disabled if added InputAttribute ReadOnly" $ do
            renderText (toHtml (Tagged (Just 2) :: Tagged
                '(Simple, Choose '[ '("1","One"),'("2","Two")] '[ReadOnly]) (Maybe Int)))
                `shouldBe` "<select disabled><option value=\"1\">One</option><option value=\"2\" selected>Two</option></select>"
    describe "Rendering with label: (Simple, l::Symbol, t::HtmlTag)" $ do
        it "render field into label" $ do
            renderText (toHtml (Tagged (Just "test") :: Tagged '(Simple, "test", Input '[]) (Maybe Text)))
                `shouldBe` "<label>test: <input value=\"test\"></label>"
            renderText (toHtml (Tagged (Just 2) :: Tagged
                '(Simple, "test", Choose '[ '("1","One"),'("2","Two")] '[ReadOnly]) (Maybe Int)))
                `shouldBe` "<label>test: <select disabled><option value=\"1\">One</option><option value=\"2\" selected>Two</option></select></label>"
    describe "Rendering as table row: (Simple, (l::Symbol, t::HtmlTag))" $ do
        it "render table row" $ do
            renderText (toHtml (Tagged (Just "test") :: Tagged '(Simple, '("test", Input '[])) (Maybe Text)))
                `shouldBe` "<tr><td>test: </td><td><input value=\"test\"></td></tr>"
            renderText (toHtml (Tagged (Just 2) :: Tagged
                '(Simple, '("test", Choose '[ '("1","One"),'("2","Two")] '[ReadOnly])) (Maybe Int)))
                `shouldBe` "<tr><td>test: </td><td><select disabled><option value=\"1\">One</option><option value=\"2\" selected>Two</option></select></td></tr>"

-- renderText (toHtml (Tagged (Just 3)
--        :: Tagged '(Simple, "test", Choose '[ '("a","aaa"), '("b", "bbb")] '[ReadOnly])
--                  (Maybe Double))
--   <div><label>test: </label><select disabled><option value="aaa">a</option><option value="bbb">b</option></select></div>

