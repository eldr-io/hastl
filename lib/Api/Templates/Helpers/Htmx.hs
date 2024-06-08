-- https://github.com/monadicsystems/lucid-htmx/
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

module Api.Templates.Helpers.Htmx where

import Data.Text (Text, pack)
import Lucid (defer_, script_, src_)
import Lucid.Base

-- | <https://htmx.org/attributes/hx-boost/>
hxBoost_ :: Text -> Attributes
hxBoost_ = makeAttributes "data-hx-boost"

-- | <https://htmx.org/attributes/hx-confirm/>
hxConfirm_ :: Text -> Attributes
hxConfirm_ = makeAttributes "data-hx-confirm"

-- | <https://htmx.org/attributes/hx-delete/>
hxDelete_ :: Text -> Attributes
hxDelete_ = makeAttributes "data-hx-delete"

-- | <https://htmx.org/attributes/hx-disable/>
hxDisable_ :: Attributes
hxDisable_ = makeAttributes "data-hx-disable" mempty

-- | <https://htmx.org/attributes/hx-encoding/>
hxEncoding_ :: Text -> Attributes
hxEncoding_ = makeAttributes "data-hx-encoding"

-- | <https://htmx.org/attributes/hx-ext/>
hxExt_ :: Text -> Attributes
hxExt_ = makeAttributes "data-hx-ext"

-- | <https://htmx.org/attributes/hx-get/>
hxGet_ :: Text -> Attributes
hxGet_ = makeAttributes "data-hx-get"

-- | <https://htmx.org/attributes/hx-headers/>
hxHeaders_ :: Text -> Attributes
hxHeaders_ = makeAttributes "data-hx-headers"

-- | <https://htmx.org/attributes/hx-history-elt/>
hxHistoryElt_ :: Attributes
hxHistoryElt_ = makeAttributes "data-hx-history-elt" mempty

-- | <https://htmx.org/attributes/hx-include/>
hxInclude_ :: Text -> Attributes
hxInclude_ = makeAttributes "data-hx-include"

-- | <https://htmx.org/attributes/hx-indicator/>
hxIndicator_ :: Text -> Attributes
hxIndicator_ = makeAttributes "data-hx-indicator"

-- | <https://htmx.org/attributes/hx-params/>
hxParams_ :: Text -> Attributes
hxParams_ = makeAttributes "data-hx-params"

-- | <https://htmx.org/attributes/hx-patch/>
hxPatch_ :: Text -> Attributes
hxPatch_ = makeAttributes "data-hx-patch"

-- | <https://htmx.org/attributes/hx-post/>
hxPost_ :: Text -> Attributes
hxPost_ = makeAttributes "data-hx-post"

-- | <https://htmx.org/attributes/hx-preserve/>
hxPreserve_ :: Text -> Attributes
hxPreserve_ = makeAttributes "data-hx-preserve"

-- | <https://htmx.org/attributes/hx-prompt/>
hxPrompt_ :: Text -> Attributes
hxPrompt_ = makeAttributes "data-hx-prompt"

-- | <https://htmx.org/attributes/hx-push-url/>
hxPushUrl_ :: Text -> Attributes
hxPushUrl_ = makeAttributes "data-hx-push-url"

-- | <https://htmx.org/attributes/hx-put/>
hxPut_ :: Text -> Attributes
hxPut_ = makeAttributes "data-hx-put"

-- | <https://htmx.org/attributes/hx-request/>
hxRequest_ :: Text -> Attributes
hxRequest_ = makeAttributes "data-hx-request"

-- | <https://htmx.org/attributes/hx-select/>
hxSelect_ :: Text -> Attributes
hxSelect_ = makeAttributes "data-hx-select"

-- | <https://htmx.org/attributes/hx-sse/>
hxSse_ :: Text -> Attributes
hxSse_ = makeAttributes "data-hx-sse"

-- | <https://htmx.org/attributes/hx-swap-oob/>
hxSwapOob_ :: Text -> Attributes
hxSwapOob_ = makeAttributes "data-hx-swap-oob"

-- | <https://htmx.org/attributes/hx-swap/>
hxSwap_ :: Text -> Attributes
hxSwap_ = makeAttributes "data-hx-swap"

-- | <https://htmx.org/attributes/hx-target/>
hxTarget_ :: Text -> Attributes
hxTarget_ = makeAttributes "data-hx-target"

-- | <https://htmx.org/attributes/hx-trigger/>
hxTrigger_ :: Text -> Attributes
hxTrigger_ = makeAttributes "data-hx-trigger"

-- | <https://htmx.org/attributes/hx-vals/>
hxVals_ :: Text -> Attributes
hxVals_ = makeAttributes "data-hx-vals"

hxTarget500_ :: Text -> Attributes
hxTarget500_ = makeAttributes "data-hx-target-500"

hxTargetError_ :: Text -> Attributes
hxTargetError_ = makeAttributes "data-hx-target-error"

-- | <https://htmx.org/attributes/hx-ws/>
hxWs_ :: Text -> Attributes
hxWs_ = makeAttributes "data-hx-ws"

ariaCurrent_ :: Text -> Attributes
ariaCurrent_ = makeAttributes "aria-current"

ariaExpanded_ :: Text -> Attributes
ariaExpanded_ = makeAttributes "aria-expanded"

ariaControls_ :: Text -> Attributes
ariaControls_ = makeAttributes "aria-controls"

ariaHidden_ :: Text -> Attributes
ariaHidden_ = makeAttributes "aria-hidden"

strokeWidth_ :: Text -> Attributes
strokeWidth_ = makeAttributes "stroke-width"

stroke_ :: Text -> Attributes
stroke_ = makeAttributes "stroke"

strokeLinecap_ :: Text -> Attributes
strokeLinecap_ = makeAttributes "stroke-linecap"

strokeLineJoin_ :: Text -> Attributes
strokeLineJoin_ = makeAttributes "stroke-linejoin"

fill_ :: Text -> Attributes
fill_ = makeAttributes "fill"

viewBox_ :: Text -> Attributes
viewBox_ = makeAttributes "viewBox"

d_ :: Text -> Attributes
d_ = makeAttributes "d"

path_ :: (Term arg result) => arg -> result
path_ = term "path"

r_ :: (Term arg result) => arg -> result
r_ = term "r"

cx_ :: (Term arg result) => arg -> result
cx_ = term "cx"

cy_ :: (Term arg result) => arg -> result
cy_ = term "cy"

circle_ :: (Term arg result) => arg -> result
circle_ = term "circle"

-- | Use this value in your @head_@ tag to use htmx in your lucid templates
useHtmx :: (Monad m) => HtmlT m ()
useHtmx = script_ [defer_ "", src_ htmxSrc] ("" :: Html ())

-- | Choose the version of Alpine.js to use using a 3-tuple representing semantic versioning
useHtmxVersion :: (Monad m) => (Int, Int, Int) -> HtmlT m ()
useHtmxVersion semVer = script_ [defer_ "", src_ $ htmxSrcWithSemVer semVer] ("" :: Html ())

useHtmxJsExt :: (Monad m) => HtmlT m ()
useHtmxJsExt = script_ [defer_ "", src_ "https://unpkg.com/htmx.org@1.9.12/dist/ext/json-enc.js"] ("" :: Html ())

useHtmxRetargetErrorsExt :: (Monad m) => HtmlT m ()
useHtmxRetargetErrorsExt = script_ [defer_ "", src_ "https://unpkg.com/htmx.org@1.9.12/dist/ext/response-targets.js"] ("" :: Html ())

htmxSrc :: Text
htmxSrc = "https://unpkg.com/htmx.org"

htmxSrcWithSemVer :: (Int, Int, Int) -> Text
htmxSrcWithSemVer (major, minor, patch) =
  htmxSrc
    <> "@"
    <> showT major
    <> "."
    <> showT minor
    <> "."
    <> showT patch

showT :: (Show a) => a -> Text
showT = pack . show
