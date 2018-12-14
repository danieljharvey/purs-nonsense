module Test.Main where

import Prelude (Unit, ($), (==), discard)
import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

import Interaction

main :: Effect Unit
main = runTest do
  suite "Interaction" do
    test "showValidation with default values" do
      Assert.assert "Always show validation" $ do
        showValidation (defaultInteraction Always) == true
      Assert.assert "Show validation after a change" $ do
        showValidation (defaultInteraction AfterChange) == false
      Assert.assert "Show validation after focus" $ do
        showValidation (defaultInteraction AfterFocus) == false
      Assert.assert "Show validation after blurrng" $ do
        showValidation (defaultInteraction AfterBlurred) == false
      Assert.assert "Show validation after clicking" $ do
        showValidation (defaultInteraction AfterClicked) == false
      Assert.assert "Show validation after submit" $ do
        showValidation (defaultInteraction AfterSubmit) == false
      Assert.assert "Show validation when not focused" $ do
        showValidation (defaultInteraction NotFocused) == true

    test "showValidation with set values" do
      Assert.assert "Show validation after a change" $ do
        let inter = defaultInteraction AfterChange
        showValidation inter { hasChanged = true} == true
      Assert.assert "Show validation after focus" $ do
        let inter = defaultInteraction AfterFocus
        showValidation inter { hasFocused = true} == true
      Assert.assert "Show validation after blurrng" $ do
        let inter = defaultInteraction AfterBlurred
        showValidation inter { hasBlurred = true} == true
      Assert.assert "Show validation after clicking" $ do
        let inter = defaultInteraction AfterClicked
        showValidation inter { hasClicked = true} == true
      Assert.assert "Show validation after submit" $ do
        let inter = defaultInteraction AfterSubmit
        showValidation inter { hasSubmitted = true} == true
      Assert.assert "Show validation when not focused" $ do
        let inter = defaultInteraction NotFocused
        showValidation inter { focused = true} == false
