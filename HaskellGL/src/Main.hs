-- ------------------------------------------------------------------------
-- HaskellGL - A first look at the Haskell gl library
-- ------------------------------------------------------------------------
--
-- See: http://dpwright.com/posts/2015/03/25/the-haskell-gl-package/
--
-- ------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnicodeSyntax #-}

import Prelude.Unicode
import Control.Monad.Unicode
import Graphics.GL
import qualified Graphics.UI.GLFW as GLFW
import Linear (V2(..), V3(..), M44, Quaternion,
         perspective, lookAt, axisAngle,
         mkTransformation, (!*!), inv33,
         column, _xyz, negated, identity)
import Data.Distributive (distribute)
import Control.Lens((^.))

import Codec.Picture (readPng, Image(Image),
       DynamicImage (ImageRGBA8))

import Control.Monad (void, when, unless, liftM2)
import Control.Applicative ((<$>), (<*>), pure)
import System.IO (hSetBuffering, stdout,
                 BufferMode(LineBuffering))
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Data.Bits((.|.))

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Foreign as T

import qualified Data.Vector  as V

import qualified Data.Vector.Storable as SV
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Marshal.Utils (with)
import Foreign.Storable (peek, sizeOf)
import Foreign.Ptr (Ptr, nullPtr, castPtr, wordPtrToPtr)

import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Cont (ContT(..), evalContT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)

-- ---------------------------------------------------------------------------
-- Error-hnadling utilities (Not used here, but useful for debugging
-- ---------------------------------------------------------------------------
getErrors :: IO[GLuint]
getErrors = do
    err <- glGetError
    if err == GL_NO_ERROR
       then return []
       else do
          errs <- getErrors
          return $ err:errs

-- | Translate a gl error code to a string
showError :: GLuint → String
showError GL_INVALID_ENUM = "GL_INVALID_ENUM"
showError GL_INVALID_VALUE = "GL_INVALID_VALUE"
showError GL_INVALID_OPERATION = "GL_INVALID_OPERATION"
showError GL_INVALID_FRAMEBUFFER_OPERATION = "GL_INVALID_FRAMEBUFFER_OPERATION"
showError GL_OUT_OF_MEMORY = "GL_OUT_OF_MEMORY"
showError GL_STACK_UNDERFLOW = "GL_STACK_UNDERFLOW"
showError GL_STACK_OVERFLOW = "GL_STACK_OVERFLOW"
showError x = "GL Error " ++ show x

-- | Print an error, when occured. Prefix gives location
printErrors :: String -> IO()
printErrors prefix = do
   es <- map showError <$> getErrors
   when (not $ null es) $
      error (prefix ++ ": " ++ show es)


main :: IO ()
main = do
   hSetBuffering stdout LineBuffering     -- flush every line immediately
                                          -- (makes debugging easier!)
   success <- GLFW.init
   if not success
      then void $ putStrLn "Failed to initialize GLFW"
      else do
        mapM_ GLFW.windowHint
          [ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'OpenGL
          , GLFW.WindowHint'ContextVersionMajor 3 
          , GLFW.WindowHint'ContextVersionMinor 1 
          , GLFW.WindowHint'OpenGLForwardCompat True
          ]
          -- MinorVersion 2 does not work !!
          {-
          , GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core -- this fails
          -}
        w <- GLFW.createWindow 480 320 "Haskell GL" Nothing Nothing
        case w of
           Nothing -> putStrLn "Failed to create window"
           -- Just win -> putStrLn "Window created"
           Just win -> do
              GLFW.makeContextCurrent w 


              putStrLn "Window created"




