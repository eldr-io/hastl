-- https://github.com/monadicsystems/lucid-alpine/
-- Copyright 2022 Monadic Systems LLC
--
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
--
--     * Neither the name of Monadic Systems LLC nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

module Api.Templates.Helpers.Alpine where

import Data.Text (Text, intercalate, pack)
import Lucid (Html, HtmlT, defer_, script_, src_)
import Lucid.Base (Attributes, makeAttributes)

{- | x-data
Declare a new Alpine component and its data for a block of HTML
-}
xData_ :: Text -> Attributes
xData_ = makeAttributes "x-data"

{-
<div x-data="{ open: false }">
    ...
</div>
-}

{- | x-bind
Dynamically set HTML attributes on an element
-}
xBind_ ::
  -- | Attributes name
  Text ->
  Text ->
  Attributes
xBind_ attr = makeAttributes ("x-bind:" <> attr)

{-
<div x-bind:class="! open ? 'hidden' : ''">
  ...
</div>
-}

{- | x-on
Listen for browser events on an element
-}
xOn_ ::
  -- | Event name
  Text ->
  Text ->
  Attributes
xOn_ event = makeAttributes ("x-on:" <> event)

{- | x-id
 - Target a specific id
-}
xId_ ::
  -- | Element id
  Text ->
  Attributes
xId_ = makeAttributes "x-id"

{-
<button x-on:click="open = ! open">
  Toggle
</button>
-}

{- | x-text
Set the text content of an element
-}
xText_ :: Text -> Attributes
xText_ = makeAttributes "x-text"

{-
<div>
  Copyright ©

  <span x-text="new Date().getFullYear()"></span>
</div>
-}

{- | x-html
Set the inner HTML of an element
-}
xHtml_ :: Text -> Attributes
xHtml_ = makeAttributes "x-html"

{-
<div x-html="(await axios.get('/some/html/partial')).data">
  ...
</div>
-}

{- | x-model
Synchronize a piece of data with an input element
-}
xModel_ ::
  -- | List of x-model modifiers
  [Text] ->
  Text ->
  Attributes
xModel_ mods = case mods of
  [] -> makeAttributes "x-model"
  _ -> makeAttributes ("x-model." <> intercalate "." mods)

{-
<div x-data="{ search: '' }">
  <input type="text" x-model="search">

  Searching for: <span x-text="search"></span>
</div>
-}

{- | x-show
Toggle the visibility of an element
-}
xShow_ :: Text -> Attributes
xShow_ = makeAttributes "x-show"

{-
<div x-show="open">
  ...
</div>
-}

{- | x-transition
Transition an element in and out using CSS transitions
-}
xTransition_ ::
  -- | Transition directive
  Maybe Text ->
  -- | List of x-transition modifiers
  [Text] ->
  Text ->
  Attributes
xTransition_ Nothing [] _ = makeAttributes "x-transition" mempty -- No directive or modifiers
xTransition_ (Just dir) [] attrVal = makeAttributes ("x-transition:" <> dir) attrVal -- Directive with custom transition classes
xTransition_ Nothing mods _ = makeAttributes ("x-transition." <> intercalate "." mods) mempty -- No directive, but with modifiers
xTransition_ (Just dir) mods _ = makeAttributes ("x-transition:" <> dir <> "." <> intercalate "." mods) mempty -- Directive with modifiers

{-
<div x-show="open" x-transition>
  ...
</div>
-}

{- | x-for
Repeat a block of HTML based on a data set
-}
xFor_ :: Text -> Attributes
xFor_ = makeAttributes "x-for"

xForKey_ :: Text -> Attributes
xForKey_ = makeAttributes ":key"

{-
<template x-for="post in posts">
  <h2 x-text="post.title"></h2>
</template>
-}

{- | x-if
Conditionally add/remove a block of HTML from the page entirely.
-}
xIf_ :: Text -> Attributes
xIf_ = makeAttributes "x-if"

{-
<template x-if="open">
  <div>...</div>
</template>
-}

{- | x-init
Run code when an element is initialized by Alpine
-}
xInit_ :: Text -> Attributes
xInit_ = makeAttributes "x-init"

{-
<div x-init="date = new Date()"></div>
-}

{- | x-effect
Execute a script each time one of its dependancies change
-}
xEffect_ :: Text -> Attributes
xEffect_ = makeAttributes "x-effect"

{-
<div x-effect="console.log('Count is '+count)"></div>
-}

{- | x-ref
Reference elements directly by their specified keys using the $refs magic property
-}
xRef_ :: Text -> Attributes
xRef_ = makeAttributes "x-ref"

{-
<input type="text" x-ref="content">

<button x-on:click="navigator.clipboard.writeText($refs.content.value)">
  Copy
</button>
-}

{- | x-cloak
Hide a block of HTML until after Alpine is finished initializing its contents
-}
xCloak_ :: Attributes
xCloak_ = makeAttributes "x-cloak" mempty

{-
<div x-cloak>
  ...
</div>
-}

{- | x-ignore
Prevent a block of HTML from being initialized by Alpine
-}
xIgnore_ :: Attributes
xIgnore_ = makeAttributes "x-ignore" mempty

{-
<div x-ignore>
  ...
</div>
-}

-- | Use this value in your @head_@ tag to use Alpine.js in your lucid templates
useAlpine :: (Monad m) => HtmlT m ()
useAlpine = script_ [defer_ "", src_ alpineSrc] ("" :: Html ())

-- | Choose the version of Alpine.js to use using a 3-tuple representing semantic versioning
useAlpineVersion :: (Monad m) => (Int, Int, Int) -> HtmlT m ()
useAlpineVersion semVer = script_ [defer_ "", src_ $ alpineSrcWithSemVer semVer] ("" :: Html ())

alpineSrc :: Text
alpineSrc = "https://unpkg.com/alpinejs"

alpineSrcWithSemVer :: (Int, Int, Int) -> Text
alpineSrcWithSemVer (major, minor, patch) =
  alpineSrc
    <> "@"
    <> showT major
    <> "."
    <> showT minor
    <> "."
    <> showT patch

showT :: (Show a) => a -> Text
showT = pack . show
