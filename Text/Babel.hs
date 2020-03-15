{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
-- | A Shakespearean module for Babel, introducing type-safe,
-- compile-time variable and url interpolation. It is exactly the same as
-- "Text.Julius", except that the template is first compiled to Javascript
-- with @npx babel@. This allows you to make use of ECMAScript 2015+ features
-- while still producing backwards compatible JavaScript.
--
-- To use this module, @npm@ must be installed on your system alongside
-- the following @npm@ packages: @npx@, @\@babel\/core@ and @\@babel\/cli@
-- alongside any other @babel@ packages and presets you wish to use.
--
-- If you interpolate variables,
-- the template is first wrapped with a function containing javascript variables representing shakespeare variables,
-- then compiled with Babel,
-- and then the value of the variables are applied to the function.
-- This means that in production the template can be compiled
-- once at compile time and there will be no dependency in your production
-- system on Babel. 
--
-- Your code:
--
-- > var xs = [1, 2, 3].map((n) => n + #{a});
-- > console.log(xs);
--
-- Final Result:
--
-- > ;(function(shakespeare_var_a){
-- >   var xs = [1, 2, 3].map(function(n) {
-- >     return n + shakespeare_var_a;
-- >   });
-- >   console.log(xs);
-- > })(#{a});
--
--
-- Further reading:
--
-- 1. Shakespearean templates: <https://www.yesodweb.com/book/shakespearean-templates>
--
-- 2. Babel: <https://babeljs.io>
--
-- 3. npm: <https://www.npmjs.com>
module Text.Babel
    ( -- * Functions
      -- ** Template-Reading Functions
      -- | These QuasiQuoter and Template Haskell methods return values of
      -- type @'JavascriptUrl' url@. See the Yesod book for details.
      babel
    , babelFile
    , babelFileReload
    , babel'
    , babelFile'
    , babelFileReload'
      -- * Babel Configuration
    , BabelOpts (..)
    , mkBabelSettings
      -- ** Babel Args
    , noBabelrc
      -- ** Babel Presets
    , presetEnv
    , presetReact
    , presetTypeScript
#ifdef TEST_EXPORT
    , babelSettings
#endif
    ) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Text.Shakespeare
import Text.Julius
import Data.List (intersperse)

-- | Example:
-- > BabelOpts ["--no-babelrc"] ["@babel/preset-react"]
-- produces the following @babel@ command.
-- > npx babel --no-babelrc --presets=@babel/preset-react
data BabelOpts = BabelOpts
    { bArgs :: [String] -- ^ Arguments to be passed to @babel@.
    , bPres :: [String] -- ^ Presets for use by @babel@.
    }

toArgList :: BabelOpts -> [String]
toArgList BabelOpts{..} = bArgs ++
    if null bPres
        then []
        else ["--presets=" ++ (concat $ intersperse "," bPres)]

-- | Babel compiles various forms of JavScript code into a backwards compatible version of JavaScript.
-- We do this compilation once at compile time to avoid needing to do it during the request.
-- We call this a preConversion because other shakespeare modules like Lucius use Haskell to compile during the request instead rather than a system call.
mkBabelSettings :: BabelOpts -> Q ShakespeareSettings
mkBabelSettings babelOpts = do
    jsettings <- javascriptSettings
    return $ jsettings
      { preConversion = Just PreConvert
          { preConvert = ReadProcess "npx" ("babel" : toArgList babelOpts)
          , preEscapeIgnoreBalanced = "'\"`"     -- don't insert backtacks for variable already inside strings or backticks.
          , preEscapeIgnoreLine = "//"           -- ignore commented lines
          , wrapInsertion = Just WrapInsertion
              { wrapInsertionIndent = Nothing
              , wrapInsertionStartBegin = ";(function("
              , wrapInsertionSeparator = ", "
              , wrapInsertionStartClose = "){"
              , wrapInsertionEnd = "})"
              , wrapInsertionAddParens = False
              --, wrapInsertionBraces = False
              }
          }
      }

-- | Babel compiles "ECMAScript 2015+ code into a backwards compatible version of JavaScript".
-- We do this compilation once at compile time to avoid needing to do it during the request.
-- We call this a preConversion because other shakespeare modules like Lucius use Haskell to compile during the request instead rather than a system call.
-- Passes the @--no-babelrc@ argument to @babel@.
babelSettings :: Q ShakespeareSettings
babelSettings = mkBabelSettings $ BabelOpts [noBabelrc] []

-- | Read inline, quasiquoted JavaScript to be compiled with Babel.
babel :: QuasiQuoter
babel = QuasiQuoter { quoteExp = \s -> do
    rs <- babelSettings
    quoteExp (shakespeare rs) s
    }

-- | Read in a JavaScript template file to be compiled with Babel.
-- This function reads the file once, at compile time.
babelFile :: FilePath -> Q Exp
babelFile fp = do
    rs <- babelSettings
    shakespeareFile rs fp

-- | Read in a JavaScript template file to be compiled with Babel.
-- This impure function uses unsafePerformIO to re-read the file
-- on every call, allowing for rapid iteration.
babelFileReload :: FilePath -> Q Exp
babelFileReload fp = do
    rs <- babelSettings
    shakespeareFileReload rs fp

-- | Same as 'babel' but allows the @babel@ command to be
-- invoked with a custom set of arguments.
babel' :: Q ShakespeareSettings -> QuasiQuoter
babel' settings = QuasiQuoter { quoteExp = \s -> do
    rs <- settings
    quoteExp (shakespeare rs) s
    }

-- | Same as 'babelFile' but allows the @babel@ command to be
-- invoked with a custom set of arguments.
babelFile' :: Q ShakespeareSettings -> FilePath -> Q Exp
babelFile' settings fp = do
    rs <- settings
    shakespeareFile rs fp

-- | Same as 'babelFileReload' but allows the @babel@ command to be
-- invoked with a custom set of arguments.
babelFileReload' :: Q ShakespeareSettings -> FilePath -> Q Exp
babelFileReload' settings fp = do
    rs <- settings
    shakespeareFileReload rs fp

-- | @--no-babelrc@
noBabelrc :: String
noBabelrc = "--no-babelrc"

-- | @\@babel\/preset-env@
presetEnv :: String
presetEnv = "@babel/preset-env"

-- | @\@babel\/preset-react@
-- Used by 'Data.Text.Babel.React'.
presetReact :: String
presetReact = "@babel/preset-react"

-- | @\@babel\/preset-typescript@
-- Used by 'Data.Text.Babel.TypeScript'.
presetTypeScript :: String
presetTypeScript = "@babel/preset-typescript"