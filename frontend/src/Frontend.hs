{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

module Frontend where

import Run (Options (Options), runFin)

import Control.Applicative (liftA2)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map.Strict as M

import Obelisk.Frontend
-- import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core hiding (checkbox)

-- import Common.Api
import Common.Route

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend =
  Frontend
    {
      _frontend_body = application,
      _frontend_head =
        do
          el "title" (text "Fin")
          elAttr
            "link"
            ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet")
            blank
  }

application ::
  (DomBuilder t m, PostBuild t m) =>
  m ()
application = do
  elId "div" "heading" $ do
    elAttr "img" ("src" =: $(static "fin_logo.jpeg")) blank
    el "h1" (text "Fin – An F Interpreter")
  elId "div" "main" $ do
    options <- optionsElement
    elId "h4" "input_heading" $ do
      elAttr "span" ("class" =: "emphasize") (text "F")
      text " code input"
    elId "h4" "output_heading" $ do
      text "Output"
    input <- fmap value $ textAreaElement $
      textAreaElementConfig_initialValue .~ "main = undefined;" $
      initialAttributes
        .~ M.mapKeys (AttributeName Nothing) ("id" =: "input") $
      def
    _ <- textAreaElementDynValue
      (runFinText <$> options <*> input)
      (
        initialAttributes
          .~ M.mapKeys (AttributeName Nothing) ("id" =: "output" <> "readonly" =: "readonly") $
        def
      )
    pure ()

optionsElement :: (DomBuilder t m) => m (Dynamic t Options)
optionsElement =
  elId "div" "options" $ do
    el "div" (text "Output")
    Options
      False
      <<$>> checkboxLabelled "parser result"
      <<*>> checkboxLabelled "compiler result"
      <<*>> checkboxLabelled "emulation step count"
      <<*>> checkboxLabelled "emulator result"

textAreaElementDynValue ::
  (DomBuilder t m, PostBuild t m) =>
  Dynamic t Text ->
  TextAreaElementConfig er t (DomBuilderSpace m) ->
  m (TextAreaElement er (DomBuilderSpace m) t)
textAreaElementDynValue valueNew config =
  do
    postBuild <- getPostBuild
    result <- textAreaElement $
      textAreaElementConfig_setValue
        .~ leftmost [updated valueNew, tag (current valueNew) postBuild] $
      config
    notReadyUntil postBuild
    pure result

runFinText :: Options -> Text -> Text
runFinText options = T.pack . runFin options . T.unpack

checkboxLabelled ::
  (DomBuilder t m) =>
  Text -> m (Dynamic t Bool)
checkboxLabelled label =
  fmap _inputElement_checked $
  elAttr "label" ("class" =: "label") $
    checkbox
    <*
    el "div" (text label)

elId :: (DomBuilder t m) => Text -> Text -> m a -> m a
elId elementTag i child = snd <$> elId' elementTag i child

elId' ::
  (DomBuilder t m) =>
  Text -> Text -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
elId' elementTag i = elAttr' elementTag ("id" =: i)

checkbox ::
  (DomBuilder t m) => m (InputElement EventResult (DomBuilderSpace m) t)
checkbox =
  inputElement
    (
      initialAttributes
        .~ M.mapKeys (AttributeName Nothing) ("type" =: "checkbox") $
      def
    )

infixl 4 <<$>>
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

infixl 4 <<*>>
(<<*>>) ::
  (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
(<<*>>) = liftA2 (<*>)
