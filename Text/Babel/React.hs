{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
-- | A Shakespearean module for React, introducing type-safe,
-- compile-time variable and url interpolation. It is exactly the same as
-- "Text.Julius", except that the template is first compiled to Javascript
-- with @npx babel@.
--
-- To use this module, @npm@ must be installed on your system alongside
-- the following @npm@ packages: @npx@, @\@babel\/core@, @\@babel\/cli@, @\@babel\/preset-react@.
--
-- You must also include the React and React-Dom scripts in which ever
-- webpages you make use of React. If you're using the Yesod web framework
-- then this can be done with widgets via 'addScriptRemote'.
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
-- > class HelloMessage extends React.Component {
-- >   render() {
-- >     return (
-- >       <div>
-- >         Hello {this.props.name}
-- >       </div>
-- >     );
-- >   }
-- > }
-- >
-- > ReactDOM.render(
-- >   <HelloMessage name={#{name}} />,
-- >   document.getElementById('hello-example')
-- > );
--
-- Final Result:
--
-- > (function(shakespeare_var_name) {
-- >   class HelloMessage extends React.Component {
-- >     render() {
-- >       return React.createElement("div", null, "Hello ", this.props.name)
-- >     }
-- >   }
-- >   ;ReactDOM.render(React.createElement(HelloMessage, {
-- >     name: shakespeare_var_name
-- >   }), document.getElementById('hello-example'))
-- > })(#{name});
--
--
-- Further reading:
--
-- 1. Shakespearean templates: <https://www.yesodweb.com/book/shakespearean-templates>
--
-- 2. Babel: <https://babeljs.io>
--
-- 3. npm: <https://www.npmjs.com>
--
-- 4. React: <https://reactjs.org>
module Text.Babel.React
    ( -- * Functions
      -- ** Template-Reading Functions
      -- | These QuasiQuoter and Template Haskell methods return values of
      -- type @'JavascriptUrl' url@. See the Yesod book for details.
      react
    , reactFile
    , reactFileReload

#ifdef TEST_EXPORT
    , reactSettings
#endif
    ) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Text.Shakespeare
import Text.Julius
import Text.Babel

-- | Babel compiles "ECMAScript 2015+ code into a backwards compatible version of JavaScript".
-- We do this compilation once at compile time to avoid needing to do it during the request.
-- We call this a preConversion because other shakespeare modules like Lucius use Haskell to compile during the request instead rather than a system call.
-- Configures @babel@ with 'noBabelrc' and 'presetReact'.
reactSettings :: Q ShakespeareSettings
reactSettings = mkBabelSettings $ BabelOpts [noBabelrc] [presetReact]

-- | Read inline, quasiquoted React to be compiled with Babel.
react :: QuasiQuoter
react = QuasiQuoter { quoteExp = \s -> do
    rs <- reactSettings
    quoteExp (shakespeare rs) s
    }

-- | Read in a React template file to be compiled with Babel.
-- This function reads the file once, at compile time.
reactFile :: FilePath -> Q Exp
reactFile fp = do
    rs <- reactSettings
    shakespeareFile rs fp

-- | Read in a React template file to be compiled with Babel.
-- This impure function uses unsafePerformIO to re-read the file
-- on every call, allowing for rapid iteration.
reactFileReload :: FilePath -> Q Exp
reactFileReload fp = do
    rs <- reactSettings
    shakespeareFileReload rs fp