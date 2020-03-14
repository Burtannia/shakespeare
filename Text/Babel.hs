{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
-- | A Shakespearean module for Babel, introducing type-safe,
-- compile-time variable and url interpolation. It is exactly the same as
-- "Text.Julius", except that the template is first compiled to Javascript
-- with @npx babel@. This allows you to make use of ECMAScript 2015+ features
-- while still producing backwards compatible JavaScript.
--
-- To use this module, @npm@ must be installed on your system alongside
-- the following @npm@ packages: @npx@, @\@babel\/core@ and @\@babel\/cli@.
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

#ifdef TEST_EXPORT
    , babelSettings
#endif
    ) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Text.Shakespeare
import Text.Julius

-- | Babel compiles "ECMAScript 2015+ code into a backwards compatible version of JavaScript".
-- We do this compilation once at compile time to avoid needing to do it during the request.
-- We call this a preConversion because other shakespeare modules like Lucius use Haskell to compile during the request instead rather than a system call.
babelSettings :: Q ShakespeareSettings
babelSettings = do
  jsettings <- javascriptSettings
  return $ jsettings
    { preConversion = Just PreConvert
        { preConvert = ReadProcess "npx" ["babel", "--no-babelrc"]
        , preEscapeIgnoreBalanced = "'\"`"     -- don't insert backtacks for variable already inside strings or backticks.
        , preEscapeIgnoreLine = "//"           -- ignore commented lines
        , wrapInsertion = Just WrapInsertion
            { wrapInsertionIndent = Nothing
            , wrapInsertionStartBegin = ";(function("
            , wrapInsertionSeparator = ", "
            , wrapInsertionStartClose = "){"
            , wrapInsertionEnd = "})"
            , wrapInsertionAddParens = False
            }
        }
    }

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